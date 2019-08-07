inherited frmChildModels: TfrmChildModels
  HelpType = htKeyword
  HelpKeyword = 'Child_Model_Dialog_Box'
  Caption = 'Child Models'
  ClientHeight = 547
  ClientWidth = 471
  ExplicitWidth = 487
  ExplicitHeight = 585
  PixelsPerInch = 120
  TextHeight = 18
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 113
    Height = 456
    Align = alLeft
    TabOrder = 0
    object tvChildModels: TTreeView
      Left = 1
      Top = 1
      Width = 111
      Height = 413
      Align = alClient
      HideSelection = False
      Indent = 20
      TabOrder = 0
      OnChange = tvChildModelsChange
      OnChanging = tvChildModelsChanging
    end
    object Panel3: TPanel
      Left = 1
      Top = 414
      Width = 111
      Height = 41
      Align = alBottom
      TabOrder = 1
      object btnAdd: TSpeedButton
        Left = 23
        Top = 6
        Width = 30
        Height = 30
        Hint = 'Create new child model'
        Glyph.Data = {
          96010000424D9601000000000000760000002800000018000000180000000100
          0400000000002001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCCCCCCFFFFFFFFFFFFF
          FFFFCCCCCCCFFFFFFFFFFFFFFFFFCCCCCCCFFFFFFFFFFFFFFFFFCCCCCCCFFFFF
          FFFFFFFFFFFFCCCCCCCFFFFFFFFFFFFFFFFFCCCCCCCFFFFFFFFFFFFFFFFFCCCC
          CCCFFFFFFFFFFCCCCCCCCCCCCCCCCCCCCCFFFCCCCCCCCCCCCCCCCCCCCCFFFCCC
          CCCCCCCCCCCCCCCCCCFFFCCCCCCCCCCCCCCCCCCCCCFFFCCCCCCCCCCCCCCCCCCC
          CCFFFCCCCCCCCCCCCCCCCCCCCCFFFCCCCCCCCCCCCCCCCCCCCCFFFFFFFFFFCCCC
          CCCFFFFFFFFFFFFFFFFFCCCCCCCFFFFFFFFFFFFFFFFFCCCCCCCFFFFFFFFFFFFF
          FFFFCCCCCCCFFFFFFFFFFFFFFFFFCCCCCCCFFFFFFFFFFFFFFFFFCCCCCCCFFFFF
          FFFFFFFFFFFFCCCCCCCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnAddClick
      end
      object btnDelete: TSpeedButton
        Left = 59
        Top = 6
        Width = 30
        Height = 30
        Hint = 'Delete selected child model'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FF0732DE0732DEFF00FF0732DE0732DEFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FF0732DE0732DEFF00FFFF00FF0732DE
          0732DE0732DEFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0732
          DE0732DEFF00FFFF00FFFF00FF0732DE0732DD0732DE0732DEFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FF0732DE0732DEFF00FFFF00FFFF00FFFF00FFFF00FF
          0534ED0732DF0732DE0732DEFF00FFFF00FFFF00FFFF00FF0732DE0732DEFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0732DE0732DE0732DDFF
          00FF0732DD0732DE0732DEFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FF0732DD0633E60633E60633E90732DCFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0633E307
          32E30534EFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FF0732DD0534ED0533E90434EF0434F5FF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0434F40534EF0533EBFF
          00FFFF00FF0434F40335F8FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FF0335FC0534EF0434F8FF00FFFF00FFFF00FFFF00FF0335FC0335FBFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FF0335FB0335FB0335FCFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FF0335FB0335FBFF00FFFF00FFFF00FFFF00FF0335FB
          0335FB0335FBFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FF0335FBFF00FFFF00FF0335FB0335FB0335FBFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0335FB0335FB
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnDeleteClick
      end
    end
  end
  object pcMain: TPageControl
    Left = 113
    Top = 0
    Width = 358
    Height = 456
    ActivePage = tabDiscretization
    Align = alClient
    TabOrder = 1
    object tabBasic: TTabSheet
      Caption = 'Basic'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        350
        423)
      object lblBottomUnit: TLabel
        Left = 4
        Top = 64
        Width = 210
        Height = 18
        Caption = 'Bottom layer group (NPLEND)'
      end
      object lblBottomLayer: TLabel
        Left = 4
        Top = 117
        Width = 290
        Height = 18
        Caption = 'Bottom layer within layer group (NPLEND)'
      end
      object lblCellCount: TLabel
        Left = 4
        Top = 170
        Width = 291
        Height = 36
        Caption = 
          'Number of child cells per parent cell along rows and columns (NC' +
          'PP)'
        WordWrap = True
      end
      object edModelName: TLabeledEdit
        Left = 4
        Top = 32
        Width = 343
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 126
        EditLabel.Height = 18
        EditLabel.Caption = 'Child model name'
        TabOrder = 0
        OnChange = edModelNameChange
      end
      object comboBottomUnit: TComboBox
        Left = 3
        Top = 85
        Width = 344
        Height = 26
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = comboBottomUnitChange
        OnExit = comboBottomUnitExit
      end
      object seBottomLayer: TJvSpinEdit
        Left = 4
        Top = 138
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 2
        OnChange = seBottomLayerChange
        OnExit = seBottomLayerExit
      end
      object seCellCount: TJvSpinEdit
        Left = 4
        Top = 209
        Width = 121
        Height = 26
        Increment = 2.000000000000000000
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 3.000000000000000000
        TabOrder = 3
        OnExit = seCellCountExit
      end
      object rgStartingHeads: TRadioGroup
        Left = 3
        Top = 248
        Width = 344
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Starting heads (ISHFLG)'
        Columns = 2
        ItemIndex = 1
        Items.Strings = (
          'Child model'
          'Parent model')
        TabOrder = 4
        OnClick = rgStartingHeadsClick
      end
    end
    object tabDiscretization: TTabSheet
      Caption = 'Discretization'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object rdgDiscretization: TRbwDataGrid4
        Left = 0
        Top = 27
        Width = 350
        Height = 396
        Align = alClient
        BevelInner = bvNone
        ColCount = 3
        FixedCols = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 1
        OnEnter = rdgDiscretizationEnter
        OnExit = rdgDiscretizationExit
        OnMouseUp = rdgDiscretizationMouseUp
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnColSize = rdgDiscretizationColSize
        ColorRangeSelection = False
        OnHorizontalScroll = rdgDiscretizationHorizontalScroll
        Columns = <
          item
            AutoAdjustRowHeights = False
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
            Format = rcf4Integer
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
        RowHeights = (
          24
          24
          24
          24
          24)
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 350
        Height = 27
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object rdeDiscretization: TRbwDataEntry
          Left = 136
          Top = -1
          Width = 65
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 0
          Text = '1'
          OnChange = rdeDiscretizationChange
          DataType = dtInteger
          Max = 1.000000000000000000
          Min = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
    end
    object tabSolution: TTabSheet
      Caption = 'Solution'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        350
        423)
      object lblMaxIterations: TLabel
        Left = 3
        Top = 63
        Width = 324
        Height = 36
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Maximum number of LGR iterations in two-way coupling (MXLGRITER)'
        WordWrap = True
      end
      object lblRelaxHeads: TLabel
        Left = 3
        Top = 221
        Width = 263
        Height = 18
        Caption = 'Relaxation factor for heads (RELAXH)'
      end
      object lblRelaxFlux: TLabel
        Left = 3
        Top = 270
        Width = 258
        Height = 18
        Caption = 'Relaxation factor for fluxes (RELAXF)'
      end
      object lblHeadClosure: TLabel
        Left = 3
        Top = 319
        Width = 264
        Height = 18
        Caption = 'Head closure criterion (HCLOSELGR)'
      end
      object lblFluxClosure: TLabel
        Left = 3
        Top = 368
        Width = 253
        Height = 18
        Caption = 'Flux closure criterion (FCLOSELGR)'
      end
      object seMaxIterations: TJvSpinEdit
        Left = 3
        Top = 102
        Width = 122
        Height = 26
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 1
        OnChange = seMaxIterationsChange
      end
      object rgPrintIterations: TRadioGroup
        Left = 0
        Top = 134
        Width = 347
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Print maximum head and flux change (IOUTLGR)'
        Items.Strings = (
          'Print to screen'
          'Print to listing file'
          'Don'#39't print')
        TabOrder = 2
        OnClick = rgPrintIterationsClick
      end
      object rdeRelaxHeads: TRbwDataEntry
        Left = 3
        Top = 242
        Width = 122
        Height = 22
        TabOrder = 3
        Text = '0'
        OnChange = rdeRelaxHeadsChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeRelaxFlux: TRbwDataEntry
        Left = 3
        Top = 291
        Width = 122
        Height = 22
        TabOrder = 4
        Text = '0'
        OnChange = rdeRelaxFluxChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeHeadClosure: TRbwDataEntry
        Left = 3
        Top = 340
        Width = 122
        Height = 22
        TabOrder = 5
        Text = '0'
        OnChange = rdeHeadClosureChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluxClosure: TRbwDataEntry
        Left = 3
        Top = 389
        Width = 122
        Height = 22
        TabOrder = 6
        Text = '0'
        OnChange = rdeFluxClosureChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rgCouplingMethod: TRadioGroup
        Left = 3
        Top = 3
        Width = 344
        Height = 54
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Coupling method (MXLGRITER)'
        Items.Strings = (
          'One way'
          'Two way')
        TabOrder = 0
        OnClick = rgCouplingMethodClick
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 456
    Width = 471
    Height = 91
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      471
      91)
    object btnHelp: TBitBtn
      Left = 120
      Top = 47
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 234
      Top = 47
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 348
      Top = 47
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 3
    end
    object cbSaveBFH: TCheckBox
      Left = 1
      Top = 2
      Width = 342
      Height = 39
      Caption = 
        'Save BFH boundary conditions (IUPBHSV, IUPBFSV, IUCBHSV, IUCBFSV' +
        ')'
      TabOrder = 0
      WordWrap = True
    end
  end
end
