inherited frameMt3dmsChemReactionPkg: TframeMt3dmsChemReactionPkg
  Width = 548
  Height = 451
  ExplicitWidth = 548
  ExplicitHeight = 451
  DesignSize = (
    548
    451)
  inherited memoComments: TMemo
    Width = 517
    ExplicitWidth = 517
  end
  object PageControl1: TPageControl [3]
    Left = 0
    Top = 162
    Width = 548
    Height = 289
    ActivePage = tabMain
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tabMain: TTabSheet
      Caption = 'Main'
      DesignSize = (
        540
        254)
      object lblElectronAcceptor: TLabel
        Left = 154
        Top = 144
        Width = 152
        Height = 20
        Caption = 'Electron acceptor (IEA)'
      end
      object lblElectronDonor: TLabel
        Left = 3
        Top = 144
        Width = 135
        Height = 20
        Caption = 'Electron donor (IED)'
      end
      object lblKineticChoice: TLabel
        Left = 397
        Top = 43
        Width = 154
        Height = 20
        Caption = 'Kinetic choice (IREACT)'
      end
      object lblReactionChoice: TLabel
        Left = 354
        Top = 118
        Width = 193
        Height = 20
        Caption = 'Reaction choice (IREACTION)'
      end
      object lblSorptionChoice: TLabel
        Left = 397
        Top = 3
        Width = 172
        Height = 20
        Caption = 'Sorption choice (ISOTHM)'
      end
      object lblStochiometricRatio: TLabel
        Left = 305
        Top = 144
        Width = 148
        Height = 20
        Caption = 'Stochiometric ratio (F)'
      end
      object rdgYieldCoefficient: TRbwDataGrid4
        Left = 3
        Top = 191
        Width = 517
        Height = 65
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 2
        FixedCols = 1
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnButtonClick = rdgYieldCoefficientButtonClick
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
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
      object cbInitialConcChoice: TCheckBox
        Left = 3
        Top = 77
        Width = 273
        Height = 34
        Caption = 
          'Specify initial concentration of adsorbed and immobile phases of' +
          ' all species (IGETSC)'
        Enabled = False
        TabOrder = 1
        WordWrap = True
      end
      object comboElectronAcceptor: TComboBox
        Left = 154
        Top = 163
        Width = 145
        Height = 28
        Style = csDropDownList
        TabOrder = 2
      end
      object comboElectronDonor: TComboBox
        Left = 3
        Top = 163
        Width = 145
        Height = 28
        Style = csDropDownList
        TabOrder = 3
      end
      object comboKineticChoice: TJvImageComboBox
        Left = 3
        Top = 40
        Width = 388
        Height = 28
        Style = csDropDownList
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 388
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemIndex = -1
        TabOrder = 4
        OnChange = comboKineticChoiceChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'First order (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Monod (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'First-order chain (3)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Zero order (100)'
          end>
      end
      object comboReactionChoice: TJvImageComboBox
        Left = 9
        Top = 117
        Width = 339
        Height = 28
        Style = csDropDownList
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 388
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemIndex = -1
        TabOrder = 5
        OnChange = comboReactionChoiceChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Instantaneous EA/ED reaction (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Kinetic EA/ED reaction (2)'
          end>
      end
      object comboSorptionChoice: TJvImageComboBox
        Left = 3
        Top = 3
        Width = 388
        Height = 28
        Style = csDropDownList
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 388
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemIndex = 7
        TabOrder = 6
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Linear (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Freundlich (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Langmuir (3)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'First order kinetic (4)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Dual domain - No sorption (5)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Dual domain - With sorption (6)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Dual domain - With different sorption contants (-6)'
          end>
      end
      object rdeStochiometricRatio: TRbwDataEntry
        Left = 305
        Top = 163
        Width = 145
        Height = 22
        TabOrder = 7
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object tabKinetic: TTabSheet
      Caption = 'Kinetic'
      ImageIndex = 1
      object lblElectronDonors: TLabel
        Left = 0
        Top = 0
        Width = 104
        Height = 20
        Caption = 'Electron donors'
      end
      object lblElectronAcceptors: TLabel
        Left = 127
        Top = 3
        Width = 122
        Height = 20
        Caption = 'Electron acceptors'
      end
      object lblSpecialCases: TLabel
        Left = 256
        Top = 0
        Width = 87
        Height = 20
        Caption = 'Special cases'
      end
      object seElectronDonors: TJvSpinEdit
        Left = 3
        Top = 19
        Width = 121
        Height = 28
        MaxValue = 2147483647.000000000000000000
        TabOrder = 0
        OnChange = seElectronDonorsChange
      end
      object seElectronAcceptors: TJvSpinEdit
        Left = 127
        Top = 19
        Width = 121
        Height = 28
        MaxValue = 2147483647.000000000000000000
        TabOrder = 1
        OnChange = seElectronAcceptorsChange
      end
      object clbSpecialCases: TJvCheckListBox
        Left = 256
        Top = 26
        Width = 121
        Height = 232
        DoubleBuffered = False
        ItemHeight = 20
        ParentDoubleBuffered = False
        TabOrder = 2
        OnClickCheck = clbSpecialCasesClickCheck
        OnEnter = clbSpecialCasesEnter
        OnExit = clbSpecialCasesExit
      end
      object cbSolidFe: TCheckBox
        Left = 383
        Top = 3
        Width = 193
        Height = 17
        Caption = 'Simulate solid phase Fe3+'
        TabOrder = 3
        WordWrap = True
      end
      object memoDonors: TMemo
        Left = 0
        Top = 46
        Width = 121
        Height = 212
        ReadOnly = True
        TabOrder = 4
      end
      object memoAcceptors: TMemo
        Left = 127
        Top = 46
        Width = 121
        Height = 212
        ReadOnly = True
        TabOrder = 5
      end
    end
    object tabSpecialCases: TTabSheet
      Caption = 'Special Cases'
      ImageIndex = 2
      object rdgSpecialCases: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 540
        Height = 254
        Align = alClient
        ColCount = 3
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
              'SOLID'
              'MAXEC'
              'STORE')
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
        ColWidths = (
          64
          64
          125)
      end
    end
    object tabElectronAcceptors: TTabSheet
      Caption = 'Elec. Accept.'
      ImageIndex = 3
      object rdgAcceptors: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 540
        Height = 254
        Align = alClient
        ColCount = 3
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
    object tabDecayRates: TTabSheet
      Caption = 'Decay Rates'
      ImageIndex = 4
      object rdgDecayRates: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 540
        Height = 254
        Align = alClient
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
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
            AutoAdjustColWidths = False
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
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
          end>
        WordWrapRowCaptions = False
      end
    end
    object tabYield: TTabSheet
      Caption = 'Yields'
      ImageIndex = 5
      object rdgYields: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 540
        Height = 254
        Align = alClient
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
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
            AutoAdjustColWidths = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
          end>
        WordWrapRowCaptions = False
      end
    end
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
        Control = comboSorptionChoice
      end
      item
        Control = comboKineticChoice
      end
      item
        Control = cbInitialConcChoice
      end
      item
        Control = comboReactionChoice
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
