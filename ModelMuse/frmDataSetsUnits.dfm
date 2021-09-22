inherited frmDataSets: TfrmDataSets
  Left = 404
  Top = 197
  HelpType = htKeyword
  HelpKeyword = 'Data_Sets_Dialog_Box'
  Caption = 'Data Sets'
  ClientHeight = 463
  ClientWidth = 636
  Font.Height = 19
  OnActivate = FormActivate
  OnClose = FormClose
  ExplicitWidth = 652
  ExplicitHeight = 502
  PixelsPerInch = 96
  TextHeight = 19
  object Splitter1: TSplitter
    Left = 311
    Top = 0
    Width = 5
    Height = 395
    Align = alRight
    ExplicitLeft = 325
    ExplicitHeight = 405
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 395
    Width = 636
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      636
      41)
    object btnOK: TBitBtn
      Left = 437
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'Apply'
      Default = True
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
      ModalResult = 1
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 534
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnCancelClick
    end
    object btnAdd: TButton
      Left = 8
      Top = 2
      Width = 73
      Height = 33
      Caption = 'Add'
      TabOrder = 0
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Left = 87
      Top = 2
      Width = 73
      Height = 33
      Caption = 'Delete'
      TabOrder = 1
      OnClick = btnDeleteClick
    end
    object btnHelp: TBitBtn
      Left = 340
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnHelpClick
    end
  end
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 436
    Width = 636
    Height = 27
    Panels = <>
    ParentColor = True
    SimplePanel = True
  end
  object tvDataSets: TTreeView
    Left = 0
    Top = 0
    Width = 311
    Height = 395
    Align = alClient
    HideSelection = False
    Indent = 21
    MultiSelect = True
    MultiSelectStyle = [msControlSelect, msShiftSelect]
    ReadOnly = True
    TabOrder = 0
    OnChange = tvDataSetsChange
    OnChanging = tvDataSetsChanging
  end
  object pcDataSets: TJvPageControl
    Left = 316
    Top = 0
    Width = 320
    Height = 395
    ActivePage = tabBasic
    Align = alRight
    TabOrder = 1
    object tabBasic: TTabSheet
      Caption = 'Basic'
      DesignSize = (
        312
        361)
      object lblName: TLabel
        Left = 3
        Top = 3
        Width = 42
        Height = 19
        Caption = 'Name'
      end
      object lblType: TLabel
        Left = 3
        Top = 61
        Width = 35
        Height = 19
        Caption = 'Type'
      end
      object lblOrientation: TLabel
        Left = 149
        Top = 61
        Width = 79
        Height = 19
        Caption = 'Orientation'
      end
      object lblEvaluatedAt: TLabel
        Left = 2
        Top = 121
        Width = 89
        Height = 19
        Caption = 'Evaluated At'
      end
      object lblUnits: TLabel
        Left = 149
        Top = 121
        Width = 36
        Height = 19
        Caption = 'Units'
      end
      object lblInterpolation: TLabel
        Left = 3
        Top = 181
        Width = 89
        Height = 19
        Caption = 'Interpolation'
      end
      object lblAnisotropy: TLabel
        Left = 149
        Top = 181
        Width = 78
        Height = 19
        Caption = 'Anisotropy'
      end
      object lblDefaultFormula: TLabel
        Left = 3
        Top = 245
        Width = 110
        Height = 19
        Caption = 'Default formula'
      end
      object edName: TRbwEdit
        Left = 3
        Top = 28
        Width = 274
        Height = 27
        TabOrder = 0
        OnExit = edNameExit
      end
      object comboType: TJvImageComboBox
        Left = 3
        Top = 86
        Width = 128
        Height = 29
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 23
        ItemIndex = 0
        TabOrder = 1
        OnChange = comboTypeChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Real'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Integer'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Boolean'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Text'
          end>
      end
      object comboOrientation: TJvImageComboBox
        Left = 149
        Top = 86
        Width = 128
        Height = 29
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 23
        ItemIndex = 0
        TabOrder = 2
        OnChange = comboOrientationChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '2D Top'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '2D Front'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '2D Side'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '3D'
          end>
      end
      object comboEvaluatedAt: TJvImageComboBox
        Left = 2
        Top = 146
        Width = 128
        Height = 29
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 23
        ItemIndex = 0
        TabOrder = 3
        OnChange = comboEvaluatedAtChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Elements'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Nodes'
          end>
      end
      object comboInterpolation: TJvImageComboBox
        Left = 3
        Top = 206
        Width = 128
        Height = 29
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 23
        ItemIndex = -1
        TabOrder = 6
        OnChange = comboInterpolationChange
        Items = <>
      end
      object rdeAnisotropy: TRbwDataEntry
        Left = 149
        Top = 204
        Width = 128
        Height = 30
        Cursor = crIBeam
        TabOrder = 5
        Text = '1'
        OnChange = rdeAnisotropyChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object btnEditFormula: TButton
        Left = 149
        Top = 240
        Width = 128
        Height = 25
        Caption = 'Edit formula'
        TabOrder = 7
        OnClick = btnEditFormulaClick
      end
      object reDefaultFormula: TRichEdit
        Left = 2
        Top = 271
        Width = 275
        Height = 87
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 8
        Zoom = 100
        OnExit = reDefaultFormulaExit
      end
      object comboUnits: TComboBox
        Left = 149
        Top = 146
        Width = 128
        Height = 27
        Style = csSimple
        TabOrder = 4
        OnChange = comboUnitsChange
        Items.Strings = (
          'Degrees'
          'Radians')
      end
    end
    object tabPHAST: TTabSheet
      Caption = 'PHAST-Interpolation'
      ImageIndex = 1
      inline framePhastInterpolation: TframePhastInterpolation
        Left = -2
        Top = 3
        Width = 296
        Height = 307
        TabOrder = 0
        TabStop = True
        ExplicitLeft = -2
        ExplicitTop = 3
        ExplicitWidth = 296
        ExplicitHeight = 307
        inherited lblDistance1: TLabel
          Left = 12
          Top = 128
          Width = 78
          Height = 19
          ExplicitLeft = 12
          ExplicitTop = 128
          ExplicitWidth = 78
          ExplicitHeight = 19
        end
        inherited lblDistance2: TLabel
          Left = 12
          Top = 188
          Width = 78
          Height = 19
          ExplicitLeft = 12
          ExplicitTop = 188
          ExplicitWidth = 78
          ExplicitHeight = 19
        end
        inherited lblValue1: TLabel
          Left = 184
          Top = 128
          Width = 54
          Height = 19
          ExplicitLeft = 184
          ExplicitTop = 128
          ExplicitWidth = 54
          ExplicitHeight = 19
        end
        inherited lblValue2: TLabel
          Left = 184
          Top = 188
          Width = 54
          Height = 19
          ExplicitLeft = 184
          ExplicitTop = 188
          ExplicitWidth = 54
          ExplicitHeight = 19
        end
        inherited lblMixtureFormula: TLabel
          Left = 12
          Top = 252
          Width = 111
          Height = 19
          ExplicitLeft = 12
          ExplicitTop = 252
          ExplicitWidth = 111
          ExplicitHeight = 19
        end
        inherited cbPhastInterpolation: TJvCheckBox
          Left = 4
          Top = 3
          Width = 245
          Height = 34
          Caption = 'Use PHAST-style interpolation for all cells'
          TabOrder = 0
          WordWrap = True
          OnClick = framePhastInterpolationcbPhastInterpolationClick
          AutoSize = False
          HotTrackFont.Charset = ANSI_CHARSET
          HotTrackFont.Height = 19
          HotTrackFont.Name = 'Arial'
          HotTrackFont.Pitch = fpVariable
          ExplicitLeft = 4
          ExplicitTop = 3
          ExplicitWidth = 245
          ExplicitHeight = 34
        end
        inherited rdeDistance1: TRbwDataEntry
          Left = 12
          Top = 153
          Height = 30
          OnChange = framePhastInterpolationrdeDistance1Change
          ExplicitLeft = 12
          ExplicitTop = 153
          ExplicitHeight = 30
        end
        inherited rdeDistance2: TRbwDataEntry
          Left = 12
          Top = 211
          Height = 30
          OnChange = framePhastInterpolationrdeDistance2Change
          ExplicitLeft = 12
          ExplicitTop = 211
          ExplicitHeight = 30
        end
        inherited rdeValue1: TRbwDataEntry
          Left = 184
          Top = 153
          Height = 30
          OnChange = framePhastInterpolationrdeValue1Change
          ExplicitLeft = 184
          ExplicitTop = 153
          ExplicitHeight = 30
        end
        inherited rdeValue2: TRbwDataEntry
          Left = 184
          Top = 214
          Height = 30
          OnChange = framePhastInterpolationrdeValue2Change
          ExplicitLeft = 184
          ExplicitTop = 214
          ExplicitHeight = 30
        end
        inherited rgInterpolationDirection: TRadioGroup
          Left = 12
          Top = 43
          Width = 272
          Height = 78
          Columns = 2
          TabOrder = 1
          OnClick = framePhastInterpolationrgInterpolationDirectionClick
          ExplicitLeft = 12
          ExplicitTop = 43
          ExplicitWidth = 272
          ExplicitHeight = 78
        end
        inherited edMixFormula: TRbwEdit
          Left = 12
          Top = 277
          Width = 272
          Height = 27
          TabOrder = 7
          OnChange = framePhastInterpolationedMixFormulaChange
          ExplicitLeft = 12
          ExplicitTop = 277
          ExplicitWidth = 272
          ExplicitHeight = 27
        end
        inherited btnEditMixtureFormula: TButton
          Left = 184
          Top = 250
          Width = 101
          TabOrder = 6
          OnClick = framePhastInterpolationbtnEditMixtureFormulaClick
          ExplicitLeft = 184
          ExplicitTop = 250
          ExplicitWidth = 101
        end
      end
    end
    object tabParameters: TTabSheet
      Caption = 'PEST Parameters'
      ImageIndex = 3
      TabVisible = False
      object cbParametersUsed: TCheckBox
        Left = 16
        Top = 17
        Width = 281
        Height = 17
        Caption = 'PEST Parameters used'
        TabOrder = 0
        OnClick = cbParametersUsedClick
      end
    end
    object tabComment: TTabSheet
      Caption = 'Comment'
      ImageIndex = 2
      object Splitter2: TSplitter
        Left = 0
        Top = 169
        Width = 312
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 181
      end
      object pnlComment: TPanel
        Left = 0
        Top = 0
        Width = 312
        Height = 169
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object lblComment: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 306
          Height = 19
          Align = alTop
          Caption = 'Comment'
          ExplicitWidth = 69
        end
        object reComment: TRichEdit
          AlignWithMargins = True
          Left = 3
          Top = 28
          Width = 306
          Height = 138
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
          Zoom = 100
          OnEnter = reCommentEnter
          OnExit = reCommentExit
        end
      end
      object pnlDescription: TPanel
        Left = 0
        Top = 174
        Width = 312
        Height = 187
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object lblAssociatedDataSets: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 306
          Height = 19
          Align = alTop
          Caption = 'Associated model data'
          ExplicitWidth = 164
        end
        object memoAssociatedDataSets: TMemo
          AlignWithMargins = True
          Left = 3
          Top = 28
          Width = 306
          Height = 156
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
      end
    end
  end
  object rpTopFormulaCompiler: TRbwParser
    Left = 16
    Top = 40
  end
  object rpFrontFormulaCompiler: TRbwParser
    Left = 48
    Top = 40
  end
  object rpSideFormulaCompiler: TRbwParser
    Left = 80
    Top = 40
  end
  object rpThreeDFormulaCompiler: TRbwParser
    Left = 112
    Top = 40
  end
  object rpTopFormulaCompilerNodes: TRbwParser
    Left = 16
    Top = 72
  end
  object rpFrontFormulaCompilerNodes: TRbwParser
    Left = 48
    Top = 72
  end
  object rpSideFormulaCompilerNodes: TRbwParser
    Left = 80
    Top = 72
  end
  object rpThreeDFormulaCompilerNodes: TRbwParser
    Left = 112
    Top = 72
  end
end
