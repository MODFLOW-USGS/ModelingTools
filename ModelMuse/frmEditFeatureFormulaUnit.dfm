inherited frmEditFeatureFormula: TfrmEditFeatureFormula
  HelpType = htKeyword
  HelpKeyword = 'Edit_Feature_Formula_Dialog_Bo'
  Caption = 'Edit Feature Formula'
  ClientHeight = 497
  ClientWidth = 593
  ExplicitWidth = 605
  ExplicitHeight = 535
  TextHeight = 18
  object spl1: TSplitter
    Left = 248
    Top = 0
    Width = 8
    Height = 463
    Align = alRight
    ExplicitLeft = 0
    ExplicitHeight = 601
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 463
    Width = 593
    Height = 34
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 462
    ExplicitWidth = 589
    DesignSize = (
      593
      34)
    object btnCancel: TBitBtn
      Left = 489
      Top = 2
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 485
    end
    object btnOK: TBitBtn
      Left = 401
      Top = 2
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Caption = 'Apply'
      Default = True
      Enabled = False
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
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
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 397
    end
    object btnHelp: TBitBtn
      Left = 313
      Top = 2
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 309
    end
  end
  object pnlTop: TPanel
    Left = 256
    Top = 0
    Width = 337
    Height = 463
    Align = alRight
    TabOrder = 1
    ExplicitLeft = 252
    ExplicitHeight = 462
    object memoFormula: TMemo
      Left = 1
      Top = 162
      Width = 335
      Height = 300
      Align = alClient
      TabOrder = 1
      OnChange = memoFormulaChange
      ExplicitHeight = 299
    end
    object pnlControls: TPanel
      Left = 1
      Top = 1
      Width = 335
      Height = 161
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblTotalObjects: TLabel
        Left = 14
        Top = 106
        Width = 116
        Height = 18
        Caption = 'Selected objects'
      end
      object lblStartingTime: TLabel
        Left = 188
        Top = 45
        Width = 88
        Height = 18
        Caption = 'Starting time'
      end
      object lblEndingTime: TLabel
        Left = 188
        Top = 77
        Width = 83
        Height = 18
        Caption = 'Ending time'
      end
      object btnEditFormula: TButton
        Left = 5
        Top = 130
        Width = 99
        Height = 25
        Caption = 'Edit formula'
        TabOrder = 3
        OnClick = btnEditFormulaClick
      end
      object comboEndingTime: TComboBox
        Left = 14
        Top = 74
        Width = 168
        Height = 26
        Enabled = False
        TabOrder = 2
      end
      object comboStartingTime: TComboBox
        Left = 14
        Top = 42
        Width = 168
        Height = 26
        Enabled = False
        TabOrder = 1
      end
      object comboAllTimes: TComboBox
        Left = 14
        Top = 10
        Width = 168
        Height = 26
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'Use all times'
        OnChange = comboAllTimesChange
        Items.Strings = (
          'Use all times'
          'Use selected times')
      end
    end
  end
  object tvFeatures: TTreeView
    Left = 0
    Top = 0
    Width = 248
    Height = 463
    Align = alClient
    HideSelection = False
    Indent = 19
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = tvFeaturesChange
    OnHint = tvFeaturesHint
    ExplicitWidth = 244
    ExplicitHeight = 462
  end
end
