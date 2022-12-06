object frameModpathParticles: TframeModpathParticles
  Left = 0
  Top = 0
  Width = 545
  Height = 311
  TabOrder = 0
  TabStop = True
  object gbParticles: TJvGroupBox
    Left = 0
    Top = 0
    Width = 545
    Height = 311
    Align = alClient
    Caption = 'Initial particle placement'
    TabOrder = 0
    Checkable = True
    PropagateEnable = True
    OnCheckBoxClick = gbParticlesCheckBoxClick
    DesignSize = (
      545
      311)
    object lblTimeCount: TLabel
      Left = 468
      Top = 245
      Width = 33
      Height = 15
      Caption = 'Count'
    end
    object sbAddTime: TSpeedButton
      Left = 400
      Top = 272
      Width = 23
      Height = 22
      Hint = 'Add row|Add a row below the bottom row.'
      Anchors = []
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
        CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbAddTimeClick
    end
    object sbInsertTime: TSpeedButton
      Left = 433
      Top = 272
      Width = 23
      Height = 22
      Hint = 'Insert row|Insert a row above the selected row.'
      Anchors = []
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
        CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbInsertTimeClick
    end
    object sbDeleteTime: TSpeedButton
      Left = 466
      Top = 272
      Width = 23
      Height = 22
      Hint = 'Delete row|Delete the selected row.'
      Anchors = []
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
        00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
        00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
        000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbDeleteTimeClick
    end
    object lblMessage: TLabel
      Left = 16
      Top = 136
      Width = 105
      Height = 60
      Caption = 
        'Image hidden because your computer may not be able to display it' +
        '.'
      Visible = False
      WordWrap = True
    end
    object rgChoice: TRadioGroup
      Left = 9
      Top = 16
      Width = 128
      Height = 128
      Caption = 'Choice'
      ItemIndex = 0
      Items.Strings = (
        'Grid'
        'Cylinder'
        'Sphere'
        'Individual'
        'Object')
      TabOrder = 1
    end
    object GLSceneViewer1: TGLSceneViewer
      Left = 9
      Top = 150
      Width = 128
      Height = 144
      Camera = GLCamera
      FieldOfView = 104.002532958984400000
      PenAsTouch = False
      TabOrder = 4
    end
    object plParticlePlacement: TJvPageList
      Left = 143
      Top = 24
      Width = 242
      Height = 241
      ActivePage = jvspGrid
      PropagateEnable = False
      object jvspGrid: TJvStandardPage
        Left = 0
        Top = 0
        Width = 242
        Height = 241
        Caption = 'jvspGrid'
        object lblX: TLabel
          Left = 64
          Top = 101
          Width = 47
          Height = 60
          Caption = 'Number of rows in X direction'
          WordWrap = True
        end
        object lblY: TLabel
          Left = 64
          Top = 136
          Width = 47
          Height = 60
          Caption = 'Number of rows in Y direction'
          WordWrap = True
        end
        object lblZ: TLabel
          Left = 64
          Top = 171
          Width = 47
          Height = 60
          Caption = 'Number of rows in Z direction'
          WordWrap = True
        end
        object cbLeftFace: TCheckBox
          Left = 9
          Top = 9
          Width = 122
          Height = 17
          Caption = 'Left face'
          TabOrder = 0
        end
        object cbRightFace: TCheckBox
          Left = 137
          Top = 9
          Width = 97
          Height = 17
          Caption = 'Right face'
          TabOrder = 1
        end
        object cbFrontFace: TCheckBox
          Left = 9
          Top = 32
          Width = 97
          Height = 17
          Caption = 'Front face'
          TabOrder = 2
        end
        object cbBackFace: TCheckBox
          Left = 137
          Top = 32
          Width = 122
          Height = 17
          Caption = 'Back face'
          TabOrder = 3
        end
        object cbBottomFace: TCheckBox
          Left = 9
          Top = 55
          Width = 122
          Height = 17
          Caption = 'Bottom face'
          TabOrder = 4
        end
        object cbTopFace: TCheckBox
          Left = 137
          Top = 55
          Width = 97
          Height = 17
          Caption = 'Top face'
          TabOrder = 5
        end
        object cbInternal: TCheckBox
          Left = 9
          Top = 78
          Width = 97
          Height = 17
          Caption = 'Internal'
          Checked = True
          State = cbChecked
          TabOrder = 6
        end
        object seX: TJvSpinEdit
          Left = 9
          Top = 98
          Width = 49
          Height = 24
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 7
        end
        object seY: TJvSpinEdit
          Left = 9
          Top = 133
          Width = 49
          Height = 24
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 8
        end
        object seZ: TJvSpinEdit
          Left = 9
          Top = 168
          Width = 49
          Height = 24
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 9
        end
      end
      object jvspCylinder: TJvStandardPage
        Left = 0
        Top = 0
        Width = 242
        Height = 241
        Caption = 'jvspCylinder'
        object lblCylParticleCount: TLabel
          Left = 72
          Top = 90
          Width = 88
          Height = 45
          Caption = 'Number of particles around cylinder'
          WordWrap = True
        end
        object lblClylLayerCount: TLabel
          Left = 72
          Top = 125
          Width = 94
          Height = 30
          Caption = 'Number of layers of particles'
          WordWrap = True
        end
        object lblCylRadius: TLabel
          Left = 72
          Top = 160
          Width = 79
          Height = 15
          Caption = 'Cylinder radius'
        end
        object rgCylinderOrientation: TRadioGroup
          Left = 3
          Top = 3
          Width = 150
          Height = 78
          Caption = 'Orientation'
          ItemIndex = 0
          Items.Strings = (
            'Vertical'
            'East-West'
            'North-South')
          TabOrder = 0
        end
        object seCylParticleCount: TJvSpinEdit
          Left = 9
          Top = 87
          Width = 57
          Height = 24
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 1.000000000000000000
          Value = 8.000000000000000000
          TabOrder = 1
        end
        object seCylLayerCount: TJvSpinEdit
          Left = 9
          Top = 122
          Width = 57
          Height = 24
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 2
        end
        object seCylRadius: TJvSpinEdit
          Left = 9
          Top = 154
          Width = 57
          Height = 24
          ButtonKind = bkClassic
          Increment = 0.100000000000000000
          MaxValue = 0.500000000000000000
          ValueType = vtFloat
          Value = 0.400000000000000000
          TabOrder = 3
        end
      end
      object jvspSphere: TJvStandardPage
        Left = 0
        Top = 0
        Width = 242
        Height = 241
        Caption = 'jvspSphere'
        object lblSpherParticleCount: TLabel
          Left = 72
          Top = 90
          Width = 88
          Height = 45
          Caption = 'Number of particles around sphere '
          WordWrap = True
        end
        object lblSpherelLayerCount: TLabel
          Left = 72
          Top = 125
          Width = 94
          Height = 30
          Caption = 'Number of layers of particles'
          WordWrap = True
        end
        object lblSphereRadius: TLabel
          Left = 72
          Top = 160
          Width = 71
          Height = 15
          Caption = 'Sphere radius'
        end
        object rgSphereOrientation: TRadioGroup
          Left = 3
          Top = 3
          Width = 150
          Height = 78
          Caption = 'Orientation'
          ItemIndex = 0
          Items.Strings = (
            'Vertical'
            'East-West'
            'North-South')
          TabOrder = 0
        end
        object seSphereParticleCount: TJvSpinEdit
          Left = 9
          Top = 87
          Width = 57
          Height = 24
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 1.000000000000000000
          Value = 8.000000000000000000
          TabOrder = 1
        end
        object seSphereLayerCount: TJvSpinEdit
          Left = 9
          Top = 122
          Width = 57
          Height = 24
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 2.000000000000000000
          Value = 5.000000000000000000
          BiDiMode = bdLeftToRight
          ParentBiDiMode = False
          TabOrder = 2
        end
        object seSphereRadius: TJvSpinEdit
          Left = 9
          Top = 157
          Width = 57
          Height = 24
          ButtonKind = bkClassic
          Increment = 0.100000000000000000
          MaxValue = 0.500000000000000000
          ValueType = vtFloat
          Value = 0.400000000000000000
          TabOrder = 3
        end
      end
      object jvspIndividual: TJvStandardPage
        Left = 0
        Top = 0
        Width = 242
        Height = 241
        Caption = 'jvspIndividual'
        object rdgSpecific: TRbwDataGrid4
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 236
          Height = 194
          Align = alClient
          ColCount = 4
          DefaultColWidth = 20
          FixedCols = 1
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
          TabOrder = 0
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnEndUpdate = rdgSpecificEndUpdate
          ColorRangeSelection = False
          Columns = <
            item
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = True
              CheckMin = True
              ComboUsed = False
              Format = rcf4Real
              LimitToList = False
              Max = 1.000000000000000000
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = True
              CheckMin = True
              ComboUsed = False
              Format = rcf4Real
              LimitToList = False
              Max = 1.000000000000000000
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = True
              CheckMin = True
              ComboUsed = False
              Format = rcf4Real
              LimitToList = False
              Max = 1.000000000000000000
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          WordWrapRowCaptions = False
        end
        object pnlBottom: TPanel
          Left = 0
          Top = 200
          Width = 242
          Height = 41
          Align = alBottom
          TabOrder = 1
          DesignSize = (
            242
            41)
          object lblCount: TLabel
            Left = 71
            Top = 14
            Width = 33
            Height = 15
            Caption = 'Count'
          end
          object sbDeleteRow: TSpeedButton
            Left = 207
            Top = 11
            Width = 23
            Height = 22
            Hint = 'Delete row|Delete the selected row.'
            Anchors = []
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
              00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
              0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
              0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
              0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
              00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
              000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
            ParentShowHint = False
            ShowHint = True
            OnClick = sbDeleteRowClick
          end
          object sbInsertRow: TSpeedButton
            Left = 174
            Top = 11
            Width = 23
            Height = 22
            Hint = 'Insert row|Insert a row above the selected row.'
            Anchors = []
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              0400000000008000000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
              FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
              CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
              FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
            ParentShowHint = False
            ShowHint = True
            OnClick = sbInsertRowClick
          end
          object sbAddRow: TSpeedButton
            Left = 141
            Top = 11
            Width = 23
            Height = 22
            Hint = 'Add row|Add a row below the bottom row.'
            Anchors = []
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              0400000000008000000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
              CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
              FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
              FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
            ParentShowHint = False
            ShowHint = True
            OnClick = sbAddRowClick
          end
          object seSpecificParticleCount: TJvSpinEdit
            Left = 3
            Top = 10
            Width = 62
            Height = 24
            CheckMinValue = True
            ButtonKind = bkClassic
            TabOrder = 0
          end
        end
      end
      object jvspBlank: TJvStandardPage
        Left = 0
        Top = 0
        Width = 242
        Height = 241
        Caption = 'jvspBlank'
      end
    end
    object seTimeCount: TJvSpinEdit
      Left = 400
      Top = 242
      Width = 62
      Height = 24
      CheckMaxValue = False
      ButtonKind = bkClassic
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      TabOrder = 5
    end
    object rdgReleaseTimes: TRbwDataGrid4
      Left = 400
      Top = 27
      Width = 129
      Height = 209
      ColCount = 2
      DefaultColWidth = 20
      FixedCols = 1
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
      TabOrder = 2
      ExtendedAutoDistributeText = False
      AutoMultiEdit = True
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = True
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnEndUpdate = rdgReleaseTimesEndUpdate
      ColorRangeSelection = False
      Columns = <
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4Real
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = True
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      WordWrapRowCaptions = False
    end
  end
  object OctTree: TRbwOctTree
    MaxPoints = 100
    XMax = 1.000000000000000000
    YMax = 1.000000000000000000
    ZMax = 1.000000000000000000
    Left = 144
    Top = 264
  end
  object GLScene1: TGLScene
    Left = 176
    Top = 264
    object GLDummyCube: TGLDummyCube
      Direction.Coordinates = {D36D79B2D7B35DBF010000BF00000000}
      PitchAngle = -120.000000000000000000
      Scale.Coordinates = {00000040000000400000004000000000}
      TurnAngle = -20.000000000000000000
      Up.Coordinates = {1C1DAFBEBC8FF0BE1155503F00000000}
      CubeSize = 1.000000000000000000
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Position.Coordinates = {0000204100002041000020410000803F}
        Shining = False
        SpotCutOff = 180.000000000000000000
      end
      object BottomPlane: TGLPlane
        Material.BackProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
        Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
        Material.FaceCulling = fcNoCull
        Direction.Coordinates = {000000002EBDBBB3000080BF00000000}
        PitchAngle = 180.000000000000000000
        Position.Coordinates = {00000000000000000000003F0000803F}
        Up.Coordinates = {00000000000080BF2EBDBB3300000000}
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
      object LeftPlane: TGLPlane
        Material.FrontProperties.Diffuse.Color = {8FC2753FCDCC4C3FD7A3303F0000803F}
        Direction.Coordinates = {0000803F000000002EBD3BB300000000}
        Position.Coordinates = {000000BF00000000000000000000803F}
        TurnAngle = 90.000000000000000000
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
      object BackPlane: TGLPlane
        Material.FrontProperties.Diffuse.Color = {EBE0E03EE4DB5B3FE4DB5B3F0000803F}
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        PitchAngle = 90.000000000000000000
        Position.Coordinates = {00000000000000BF000000000000803F}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
      object GLCylinder1: TGLCylinder
        Position.Coordinates = {0000003F00000000000000BF0000803F}
        BottomRadius = 0.009999999776482582
        Height = 1.000000000000000000
        TopRadius = 0.009999999776482582
      end
      object GLCylinder2: TGLCylinder
        Direction.Coordinates = {000000000000803F2EBD3BB300000000}
        PitchAngle = 90.000000000000000000
        Position.Coordinates = {0000003F0000003F000000000000803F}
        Up.Coordinates = {000000002EBD3BB3000080BF00000000}
        BottomRadius = 0.009999999776482582
        Height = 1.000000000000000000
        TopRadius = 0.009999999776482582
      end
      object GLCylinder3: TGLCylinder
        Position.Coordinates = {000000000000003F000000BF0000803F}
        RollAngle = 90.000000000000000000
        Up.Coordinates = {000080BF2EBD3BB30000000000000000}
        BottomRadius = 0.009999999776482582
        Height = 1.000000000000000000
        TopRadius = 0.009999999776482582
      end
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000803F00000000000040400000803F}
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {00000000000000000000803F00000000}
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLDummyCube
      Position.Coordinates = {0000000000000000000020410000803F}
    end
  end
end
