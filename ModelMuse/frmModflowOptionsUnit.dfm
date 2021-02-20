inherited frmModflowOptions: TfrmModflowOptions
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Options_Dialog_Box'
  Caption = 'MODFLOW Options'
  ClientHeight = 550
  ClientWidth = 543
  ExplicitWidth = 559
  ExplicitHeight = 589
  PixelsPerInch = 96
  TextHeight = 18
  object pcOptions: TPageControl
    Left = 0
    Top = 0
    Width = 543
    Height = 468
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    OnChange = pcOptionsChange
    object TabSheet1: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Description_Tab'
      Caption = 'Description'
      DesignSize = (
        535
        435)
      object Label3: TLabel
        Left = 8
        Top = 1
        Width = 92
        Height = 18
        Caption = 'Project name'
      end
      object Label4: TLabel
        Left = 8
        Top = 55
        Width = 34
        Height = 18
        Caption = 'Date'
      end
      object Label2: TLabel
        Left = 135
        Top = 55
        Width = 57
        Height = 18
        Caption = 'Modeler'
      end
      object Label1: TLabel
        Left = 8
        Top = 109
        Width = 152
        Height = 18
        Caption = 'Description of project.'
      end
      object edProjectName: TEdit
        Left = 8
        Top = 24
        Width = 520
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnExit = edProjectNameExit
      end
      object edDate: TEdit
        Left = 8
        Top = 78
        Width = 121
        Height = 26
        TabOrder = 1
        OnExit = edDateExit
      end
      object edModeler: TEdit
        Left = 135
        Top = 78
        Width = 393
        Height = 26
        TabOrder = 2
        OnExit = edModelerExit
      end
      object memoComments: TMemo
        Left = 12
        Top = 130
        Width = 520
        Height = 296
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 3
        WordWrap = False
      end
    end
    object TabSheet2: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Options_Tab'
      Caption = 'Options'
      ImageIndex = 1
      DesignSize = (
        535
        435)
      object Label5: TLabel
        Left = 154
        Top = 111
        Width = 275
        Height = 18
        Caption = 'Head value for inactive cells (HNOFLO) '
      end
      object Label6: TLabel
        Left = 151
        Top = 265
        Width = 126
        Height = 18
        Margins.Left = 0
        Caption = 'Time unit (ITMUNI)'
      end
      object Label7: TLabel
        Left = 151
        Top = 233
        Width = 145
        Height = 18
        Margins.Left = 0
        Caption = 'Length unit (LENUNI)'
      end
      object Label8: TLabel
        Left = 154
        Top = 143
        Width = 311
        Height = 18
        Caption = 'Head value for cells that become dry (HDRY) '
      end
      object lblInitialHeads: TLabel
        Left = 3
        Top = 320
        Width = 299
        Height = 18
        Caption = 'Binary file containing initial heads (optional)'
      end
      object lbl1: TLabel
        Left = 95
        Top = 167
        Width = 419
        Height = 36
        Caption = 
          'Budget percent discrepancy that will cause execution to stop (ST' +
          'OPER) '
        WordWrap = True
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 3
        Width = 529
        Height = 102
        Caption = 'Basic package options'
        TabOrder = 0
        object cbPRINTTIME: TJvCheckBox
          Left = 3
          Top = 43
          Width = 437
          Height = 18
          Caption = 'Print the start time, end time, and elapsed time (PRINTTIME)'
          Checked = True
          State = cbChecked
          TabOrder = 1
          LinkedControls = <>
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = 17
          HotTrackFont.Name = 'Microsoft Sans Serif'
          HotTrackFont.Pitch = fpVariable
          HotTrackFont.Style = []
        end
        object cbCHTOCH: TJvCheckBox
          Left = 3
          Top = 19
          Width = 461
          Height = 18
          Caption = 'Calculate flow between adjacent constant-head cells (CHTOCH)'
          Checked = True
          State = cbChecked
          TabOrder = 0
          LinkedControls = <>
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = 17
          HotTrackFont.Name = 'Microsoft Sans Serif'
          HotTrackFont.Pitch = fpVariable
          HotTrackFont.Style = []
        end
        object cbStopError: TJvCheckBox
          Left = 3
          Top = 67
          Width = 508
          Height = 32
          Caption = 
            'Continue running even if the solver closure criteria are not met' +
            '. (STOPERROR)'
          Checked = True
          State = cbChecked
          TabOrder = 2
          WordWrap = True
          OnClick = cbStopErrorClick
          LinkedControls = <>
          AutoSize = False
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = 17
          HotTrackFont.Name = 'Microsoft Sans Serif'
          HotTrackFont.Pitch = fpVariable
          HotTrackFont.Style = []
        end
      end
      object rdeHNOFLO: TRbwDataEntry
        Left = 3
        Top = 111
        Width = 145
        Height = 22
        TabOrder = 1
        Text = '-1e20'
        OnExit = rdeHNOFLOExit
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object comboTimeUnit: TJvComboBox
        Left = 3
        Top = 261
        Width = 145
        Height = 26
        Style = csDropDownList
        TabOrder = 6
        Text = 'seconds (1)'
        OnChange = comboTimeUnitChange
        Items.Strings = (
          'undefined (0)'
          'seconds (1)'
          'minutes (2)'
          'hours (3)'
          'days (4)'
          'years (5)')
        ItemIndex = 1
      end
      object comboLengthUnit: TJvComboBox
        Left = 3
        Top = 229
        Width = 145
        Height = 26
        Style = csDropDownList
        TabOrder = 5
        Text = 'meters (2)'
        OnChange = comboLengthUnitChange
        Items.Strings = (
          'undefined (0)'
          'feet (1)'
          'meters (2)'
          'centimeters (3)')
        ItemIndex = 2
      end
      object rdeHDRY: TRbwDataEntry
        Left = 3
        Top = 139
        Width = 145
        Height = 22
        TabOrder = 2
        Text = '-2e20'
        OnExit = rdeHDRYExit
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object cbOpenInTextEditor: TJvCheckBox
        Left = 3
        Top = 206
        Width = 462
        Height = 18
        Caption = 'Open listing file in text editor when model is done.'
        Checked = True
        State = cbChecked
        TabOrder = 4
        LinkedControls = <>
        AutoSize = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = 17
        HotTrackFont.Name = 'Microsoft Sans Serif'
        HotTrackFont.Pitch = fpVariable
        HotTrackFont.Style = []
      end
      object feInitialHeads: TJvFilenameEdit
        Left = 3
        Top = 341
        Width = 484
        Height = 26
        Filter = 'Binary head files (*.bhd)|*.bhd'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
        Text = ''
        OnChange = feInitialHeadsChange
      end
      object edMasUnit: TLabeledEdit
        Left = 3
        Top = 293
        Width = 145
        Height = 26
        EditLabel.Width = 127
        EditLabel.Height = 18
        EditLabel.Caption = 'Mass unit (MUNIT)'
        LabelPosition = lpRight
        MaxLength = 4
        TabOrder = 7
      end
      object rdeStopErrorCriterion: TRbwDataEntry
        Left = 3
        Top = 174
        Width = 86
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbWriteBinaryGridFile: TCheckBox
        Left = 3
        Top = 396
        Width = 409
        Height = 17
        Caption = 'Write binary grid file (MODFLOW-6) (Inverse of NOGRB option)'
        TabOrder = 9
      end
      object cbUseGsflowFormat: TCheckBox
        Left = 3
        Top = 373
        Width = 274
        Height = 17
        Caption = 'Use GSFLOW format'
        TabOrder = 10
      end
    end
    object tabWetting: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Wetting_Tab'
      Caption = 'Wetting'
      ImageIndex = 2
      object lblWetFact: TLabel
        Left = 3
        Top = 35
        Width = 241
        Height = 36
        Caption = 'Wetting Factor (WETFCT) (usually between zero and one)'
        Enabled = False
        WordWrap = True
      end
      object lblCheckDry: TLabel
        Left = 8
        Top = 107
        Width = 303
        Height = 18
        Caption = 'Iterations to  check for wetting cells (IWETIT)'
        Enabled = False
      end
      object lblWettingEquation: TLabel
        Left = 8
        Top = 163
        Width = 271
        Height = 18
        Caption = 'Equation for Rewetting Cells (IHDWET)'
        Enabled = False
      end
      object lblWettingDataSets: TLabel
        Left = 8
        Top = 235
        Width = 520
        Height = 36
        Caption = 
          'You will need to have at least one unconfined or convertible lay' +
          'er and assign non-zero values to the Wet_Dry_Threshold and Wet_D' +
          'ry_Flag data sets'
        Visible = False
        WordWrap = True
      end
      object rdeWettingFact: TRbwDataEntry
        Left = 8
        Top = 75
        Width = 92
        Height = 22
        Hint = 'This affects model stability'
        HelpContext = 290
        Color = clBtnFace
        Enabled = False
        TabOrder = 1
        Text = '0.5'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seCheckDry: TJvSpinEdit
        Left = 8
        Top = 130
        Width = 92
        Height = 26
        CheckMaxValue = False
        ButtonKind = bkClassic
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 2
      end
      object comboWettingEquation: TJvImageComboBox
        Left = 8
        Top = 186
        Width = 305
        Height = 28
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 305
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 22
        ItemIndex = -1
        TabOrder = 3
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'h = BOT + WETFCT (hn-BOT) (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'h = BOT + WETFCT (WETDRY) (1)'
          end>
      end
      object cbWetting: TCheckBox
        Left = 8
        Top = 12
        Width = 153
        Height = 17
        Caption = 'Wetting active'
        TabOrder = 0
        OnClick = cbWettingClick
      end
      object cbNewton: TCheckBox
        Left = 3
        Top = 331
        Width = 382
        Height = 17
        Caption = 'Use Newton formulation (MODFLOW-6)'
        TabOrder = 4
        OnClick = cbNewtonClick
      end
      object cbUnderRelaxation: TCheckBox
        Left = 3
        Top = 354
        Width = 406
        Height = 17
        Caption = 'Use Under_Relaxation option (MODFLOW-6)'
        TabOrder = 5
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 509
    Width = 543
    Height = 41
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      543
      41)
    object btnHelp: TBitBtn
      Left = 249
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 346
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        04000000000068010000120B0000120B00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      ModalResult = 1
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 441
      Top = 4
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object pnlModel: TPanel
    Left = 0
    Top = 468
    Width = 543
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      543
      41)
    object lblModel: TLabel
      Left = 489
      Top = 11
      Width = 43
      Height = 18
      Anchors = [akTop, akRight]
      Caption = 'Model'
    end
    object comboModel: TComboBox
      Left = 8
      Top = 8
      Width = 475
      Height = 26
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = comboModelChange
    end
  end
  object rconWet: TRbwController
    ControlList = <
      item
        Control = comboWettingEquation
      end
      item
        Control = seCheckDry
      end
      item
        Control = rdeWettingFact
      end
      item
        Control = lblWetFact
      end
      item
        Control = lblWettingEquation
      end
      item
        Control = lblCheckDry
      end>
    Enabled = False
    Left = 232
    Top = 16
  end
end
