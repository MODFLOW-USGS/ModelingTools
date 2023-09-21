inherited frmEditFeatureFormula: TfrmEditFeatureFormula
  HelpType = htKeyword
  HelpKeyword = 'Edit_Feature_Formula_Dialog_Bo'
  Caption = 'Edit Feature Formula'
  ClientHeight = 496
  ClientWidth = 610
  ExplicitWidth = 622
  ExplicitHeight = 534
  TextHeight = 18
  object spl1: TSplitter
    Left = 301
    Top = 0
    Width = 8
    Height = 408
    ExplicitLeft = 0
    ExplicitHeight = 601
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 408
    Width = 610
    Height = 88
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 407
    ExplicitWidth = 606
    DesignSize = (
      610
      88)
    object btnCancel: TBitBtn
      Left = 518
      Top = 42
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 514
    end
    object btnOK: TBitBtn
      Left = 430
      Top = 42
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
      ExplicitLeft = 426
    end
    object btnHelp: TBitBtn
      Left = 342
      Top = 42
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 338
    end
    object rgChoice: TRadioGroup
      Left = 16
      Top = 6
      Width = 313
      Height = 75
      Caption = 'Edit choice'
      ItemIndex = 0
      Items.Strings = (
        'Apply the same formula to all objects'
        'Apply a separate formula to each object.')
      TabOrder = 3
      OnClick = rgChoiceClick
    end
  end
  object tvFeatures: TTreeView
    Left = 0
    Top = 0
    Width = 301
    Height = 408
    Align = alLeft
    HideSelection = False
    Indent = 19
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = tvFeaturesChange
    OnHint = tvFeaturesHint
    ExplicitHeight = 407
  end
  object jvplEdit: TJvPageList
    Left = 309
    Top = 0
    Width = 301
    Height = 408
    ActivePage = JvStandardPage2
    PropagateEnable = False
    Align = alClient
    ExplicitWidth = 297
    ExplicitHeight = 407
    object JvStandardPage1: TJvStandardPage
      Left = 0
      Top = 0
      Width = 301
      Height = 408
      Caption = 'JvStandardPage1'
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 301
        Height = 408
        Align = alClient
        TabOrder = 0
        object memoFormula: TMemo
          Left = 1
          Top = 162
          Width = 299
          Height = 245
          Align = alClient
          TabOrder = 1
          OnChange = memoFormulaChange
        end
        object pnlControls: TPanel
          Left = 1
          Top = 1
          Width = 299
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
    end
    object JvStandardPage2: TJvStandardPage
      Left = 0
      Top = 0
      Width = 301
      Height = 408
      Caption = 'JvStandardPage2'
      ExplicitWidth = 297
      ExplicitHeight = 407
      inline frameObjectProperties: TframeGrid
        Left = 0
        Top = 81
        Width = 301
        Height = 327
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 297
        ExplicitHeight = 407
        inherited Panel: TPanel
          Top = 286
          ExplicitTop = 366
          ExplicitWidth = 297
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Height = 286
          ColCount = 3
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
              ComboUsed = True
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
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = True
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
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
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
            end>
          ExplicitWidth = 297
          ExplicitHeight = 366
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 301
        Height = 81
        Align = alTop
        TabOrder = 1
        object rgTreatment: TRadioGroup
          Left = 6
          Top = 2
          Width = 185
          Height = 73
          Caption = 'Treatment'
          ItemIndex = 0
          Items.Strings = (
            'Append new data'
            'Replace all data')
          TabOrder = 0
        end
      end
    end
  end
end
