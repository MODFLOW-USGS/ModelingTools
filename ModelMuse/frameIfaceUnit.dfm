object frameIface: TframeIface
  Left = 0
  Top = 0
  Width = 288
  Height = 236
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  TabStop = True
  object gbIface: TGroupBox
    Left = 0
    Top = 0
    Width = 288
    Height = 236
    Align = alClient
    Caption = 'Flow into boundary cell (IFACE)'
    TabOrder = 0
    object lblMessage: TLabel
      Left = 120
      Top = 71
      Width = 145
      Height = 72
      Caption = 
        'Image hidden because your computer may not be able to display it' +
        '.'
      Visible = False
      WordWrap = True
    end
    object rbBottom: TJvRadioButton
      Tag = 5
      Left = 3
      Top = 183
      Width = 94
      Height = 18
      Alignment = taLeftJustify
      Caption = 'Bottom (5)'
      TabOrder = 7
      OnClick = rbHorizontalClick
      LinkedControls = <>
    end
    object rbFront: TJvRadioButton
      Tag = 3
      Left = 3
      Top = 135
      Width = 80
      Height = 18
      Alignment = taLeftJustify
      Caption = 'Front (3)'
      TabOrder = 5
      OnClick = rbHorizontalClick
      LinkedControls = <>
    end
    object rbRight: TJvRadioButton
      Tag = 2
      Left = 3
      Top = 111
      Width = 80
      Height = 18
      Alignment = taLeftJustify
      Caption = 'Right (2)'
      TabOrder = 4
      OnClick = rbHorizontalClick
      LinkedControls = <>
    end
    object rbLeft: TJvRadioButton
      Tag = 1
      Left = 3
      Top = 87
      Width = 70
      Height = 18
      Alignment = taLeftJustify
      Caption = 'Left (1)'
      TabOrder = 3
      OnClick = rbHorizontalClick
      LinkedControls = <>
    end
    object rbTop: TJvRadioButton
      Tag = 6
      Left = 3
      Top = 207
      Width = 69
      Height = 18
      Alignment = taRightJustify
      Caption = 'Top (6)'
      TabOrder = 8
      OnClick = rbHorizontalClick
      LinkedControls = <>
    end
    object rbBack: TJvRadioButton
      Tag = 4
      Left = 3
      Top = 159
      Width = 80
      Height = 18
      Alignment = taLeftJustify
      Caption = 'Back (4)'
      TabOrder = 6
      OnClick = rbHorizontalClick
      LinkedControls = <>
    end
    object rbInternal: TJvRadioButton
      Left = 3
      Top = 63
      Width = 93
      Height = 18
      Alignment = taLeftJustify
      Caption = 'Internal (0)'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rbHorizontalClick
      LinkedControls = <>
    end
    object rbHorizontal: TJvRadioButton
      Tag = -1
      Left = 3
      Top = 21
      Width = 166
      Height = 44
      Alignment = taLeftJustify
      Caption = 'Horizontal faces next to inactive cells (-1)'
      TabOrder = 0
      WordWrap = True
      OnClick = rbHorizontalClick
      AutoSize = False
      LinkedControls = <>
    end
    object glsvViewer: TGLSceneViewer
      Left = 103
      Top = 71
      Width = 176
      Height = 146
      Camera = GLCamera1
      FieldOfView = 111.183052062988300000
      TabOrder = 2
    end
  end
  object glsIface: TGLScene
    Left = 120
    Top = 16
    object GLDummyCube1: TGLDummyCube
      Direction.Coordinates = {CAC431BE27DC82BB5C1C7C3F00000000}
      Scale.Coordinates = {00000041000000410000004100000000}
      TurnAngle = -20.000000000000000000
      Up.Coordinates = {EC65BCBCABEE7F3FA89E092E00000000}
      CubeSize = 1.000000000000000000
      object LeftFace: TGLPlane
        Material.BackProperties.Diffuse.Color = {000000000000803FF8FEFE3E0000803F}
        Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
        Material.FaceCulling = fcNoCull
        Direction.Coordinates = {2EBDBBB300000000000080BF00000000}
        Position.Coordinates = {00000000000000000000C03F0000803F}
        TurnAngle = 180.000000000000000000
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
      object GLCube1: TGLCube
        Position.Coordinates = {00000000000000000000C0BF0000803F}
        TurnAngle = -20.000000000000000000
        Visible = False
      end
      object GLLightSource2: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000E0400000A040000000C10000803F}
        Specular.Color = {6666E63E6666E63E6666E63E0000803F}
        SpotCutOff = 180.000000000000000000
        object GLSphere1: TGLSphere
          Material.FrontProperties.Diffuse.Color = {000000000000003F0000003F0000803F}
          Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
          Radius = 0.500000000000000000
        end
      end
      object FrontFace: TGLPlane
        Direction.Coordinates = {0000803F000000000000000000000000}
        Position.Coordinates = {0000003F000000000000803F0000803F}
        TurnAngle = 90.000000000000000000
        Visible = False
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
      object TopFace: TGLPlane
        Direction.Coordinates = {000000000000803F0000000000000000}
        PitchAngle = 90.000000000000000000
        Position.Coordinates = {000000000000003F0000803F0000803F}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Visible = False
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
      object BottomFace: TGLPlane
        Direction.Coordinates = {004474990000803F2EBD3BB400000000}
        PitchAngle = 90.000000000000000000
        Position.Coordinates = {00000000000000BF0000803F0000803F}
        Up.Coordinates = {00004A1D2EBD3BB4000080BF00000000}
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
      object RightFace: TGLPlane
        VisibilityCulling = vcNone
        Direction.Coordinates = {0000000000000000000080BF00000000}
        Position.Coordinates = {00000000000000000000003F0000803F}
        TurnAngle = 180.000000000000000000
        Visible = False
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
      object BackFace: TGLPlane
        Material.BackProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
        Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
        Direction.Coordinates = {0000803F000000000000000000000000}
        Position.Coordinates = {000000BF000000000000803F0000803F}
        TurnAngle = 90.000000000000000000
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
      object CentralSphere: TGLSphere
        Material.BackProperties.Diffuse.Color = {000000000000803FF8FEFE3E0000803F}
        Material.FrontProperties.Diffuse.Color = {000000000000803FF8FEFE3E0000803F}
        Position.Coordinates = {00000000000000000000803F0000803F}
        Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
        Radius = 0.500000000000000000
      end
      object Tube1: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000003F0000803F}
        Position.Coordinates = {0000003F000000000000003F0000803F}
        BottomRadius = 0.029999999329447750
        Height = 1.000000000000000000
        TopRadius = 0.029999999329447750
      end
      object Tube2: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000003F0000803F}
        Position.Coordinates = {000000000000003F0000003F0000803F}
        RollAngle = 90.000000000000000000
        Up.Coordinates = {000080BF2EBD3BB30000000000000000}
        BottomRadius = 0.029999999329447750
        Height = 1.000000000000000000
        TopRadius = 0.029999999329447750
      end
      object Tube3: TGLCylinder
        Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000003F0000803F}
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        PitchAngle = 90.000000000000000000
        Position.Coordinates = {0000003F0000003F0000803F0000803F}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        BottomRadius = 0.029999999329447750
        Height = 1.000000000000000000
        TopRadius = 0.029999999329447750
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = CentralSphere
      Position.Coordinates = {0000F04100002041000000000000803F}
    end
  end
end
