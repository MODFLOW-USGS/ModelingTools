inherited frmMeshGenerationControlVariables: TfrmMeshGenerationControlVariables
  HelpType = htKeyword
  HelpKeyword = 'Mesh_Generation_Control_Variab'
  Caption = 'Mesh Generation Control Variables'
  ClientHeight = 401
  ClientWidth = 415
  ExplicitWidth = 431
  ExplicitHeight = 440
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 356
    Width = 415
    Height = 45
    Align = alBottom
    TabOrder = 2
    object btnHelp: TBitBtn
      Left = 127
      Top = 6
      Width = 89
      Height = 33
      HelpType = htKeyword
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 222
      Top = 6
      Width = 89
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 318
      Top = 6
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 3
    end
    object btnResetDefaults: TButton
      Left = 6
      Top = 6
      Width = 115
      Height = 33
      Caption = 'Reset Defaults'
      TabOrder = 0
      OnClick = btnResetDefaultsClick
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 415
    Height = 145
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object rgMethod: TRadioGroup
      Left = 0
      Top = 0
      Width = 415
      Height = 57
      Align = alTop
      Caption = 'Mesh generation method'
      Columns = 2
      Items.Strings = (
        'Fishnet'
        'Irregular'
        'Gmsh'
        'Geompack++')
      TabOrder = 0
      OnClick = rgMethodClick
    end
    object rgRenumberingMethod: TRadioGroup
      Left = 0
      Top = 57
      Width = 415
      Height = 88
      Align = alClient
      Caption = 'Renumbering method'
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Cuthill and McKee (1969)'
        'Sloan and Randolph (1983)')
      TabOrder = 1
    end
  end
  object jvplMesh: TJvPageList
    Left = 0
    Top = 145
    Width = 415
    Height = 211
    ActivePage = jvspGmsh
    PropagateEnable = False
    Align = alClient
    object jvspFishnet: TJvStandardPage
      Left = 0
      Top = 0
      Width = 415
      Height = 211
    end
    object jvspIrregular: TJvStandardPage
      Left = 0
      Top = 0
      Width = 415
      Height = 211
      object rdgControlVariables: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 415
        Height = 211
        Align = alClient
        ColCount = 2
        FixedCols = 1
        RowCount = 8
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        ColorSelectedRow = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
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
            ButtonFont.Height = -13
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
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
    end
    object jvspGmsh: TJvStandardPage
      Left = 0
      Top = 0
      Width = 415
      Height = 211
      DesignSize = (
        415
        211)
      object htlblGmsh: TJvHTLabel
        Left = 127
        Top = 6
        Width = 116
        Height = 19
        Caption = '<a href="https://gmsh.info/">https://gmsh.info/</a>'
        SuperSubScriptRatio = 0.666666666666666600
      end
      object lblGmsh: TLabel
        Left = 6
        Top = 6
        Width = 103
        Height = 18
        Caption = 'Gmsh location '
      end
      object fedGmsh: TJvFilenameEdit
        Left = 9
        Top = 30
        Width = 403
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedGmshChange
      end
    end
    object jvspGeomPack: TJvStandardPage
      Left = 0
      Top = 0
      Width = 415
      Height = 211
      Caption = 'jvspGeomPack'
      object rrdgGeompackOptions: TRbwRowDataGrid
        Left = 0
        Top = 65
        Width = 415
        Height = 146
        Align = alClient
        ColCount = 2
        FixedCols = 1
        RowCount = 15
        FixedRows = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goAlwaysShowEditor]
        TabOrder = 1
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBlack
        ColorRangeSelection = False
        ColorSelectedColumn = False
        Columns = <
          item
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustColWidths = True
          end>
        Rows = <
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'Minimum angle'
              'Radius ratio'
              'Mean ratio'
              'Quadratic mean ratio')
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Boolean
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
          end>
        ColWidths = (
          64
          168)
      end
      object pnlGeompack: TPanel
        Left = 0
        Top = 0
        Width = 415
        Height = 65
        Align = alTop
        TabOrder = 0
        DesignSize = (
          415
          65)
        object lblGeompack: TLabel
          Left = 6
          Top = 6
          Width = 157
          Height = 18
          Caption = 'Geompack++ location '
        end
        object htlblGeompack: TJvHTLabel
          Left = 191
          Top = 6
          Width = 202
          Height = 19
          Caption = 
            '<a href="http://members.shaw.ca/bjoe/">http://members.shaw.ca/bj' +
            'oe/</a>'
          SuperSubScriptRatio = 0.666666666666666600
        end
        object fedGeompack: TJvFilenameEdit
          Left = 6
          Top = 31
          Width = 403
          Height = 26
          Filter = 
            'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
            '.*)|*.*'
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = ''
          OnChange = fedGeompackChange
        end
      end
    end
  end
end
