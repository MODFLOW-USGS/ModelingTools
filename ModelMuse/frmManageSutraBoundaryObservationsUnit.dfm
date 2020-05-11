inherited frmManageSutraBoundaryObservations: TfrmManageSutraBoundaryObservations
  Caption = 'frmManageSutraBoundaryObservations'
  ClientHeight = 461
  ClientWidth = 630
  ExplicitWidth = 646
  ExplicitHeight = 500
  PixelsPerInch = 96
  TextHeight = 18
  object spltr1: TJvNetscapeSplitter
    Left = 121
    Top = 0
    Height = 420
    Align = alLeft
    MinSize = 1
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitTop = -124
    ExplicitHeight = 350
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 420
    Width = 630
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitLeft = -368
    ExplicitTop = 185
    ExplicitWidth = 792
    DesignSize = (
      630
      41)
    object btnHelp: TBitBtn
      Left = 349
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 511
    end
    object btnCancel: TBitBtn
      Left = 535
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 4
      ExplicitLeft = 697
    end
    object btnOk: TBitBtn
      Left = 442
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 3
      ExplicitLeft = 604
    end
    object btnDeleteObservation: TButton
      Left = 88
      Top = 6
      Width = 77
      Height = 27
      Anchors = [akLeft, akBottom]
      Caption = 'Delete'
      Enabled = False
      TabOrder = 1
    end
    object btnAddObservation: TButton
      Left = 5
      Top = 6
      Width = 77
      Height = 27
      Anchors = [akLeft, akBottom]
      Caption = 'Add'
      TabOrder = 0
    end
  end
  object tvFluxObservations: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 420
    Align = alLeft
    HideSelection = False
    Indent = 20
    ReadOnly = True
    TabOrder = 1
    ExplicitTop = -124
    ExplicitHeight = 350
  end
  object pnlMain: TPanel
    Left = 131
    Top = 0
    Width = 499
    Height = 420
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = -31
    ExplicitWidth = 661
    ExplicitHeight = 350
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 499
      Height = 65
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 491
      object lblObservationName: TLabel
        Left = 6
        Top = 6
        Width = 185
        Height = 18
        Caption = 'Observation location name'
      end
      object lblTreatment: TLabel
        Left = 200
        Top = 6
        Width = 69
        Height = 18
        Caption = 'Treatment'
      end
      object edObservationName: TJvEdit
        Left = 2
        Top = 30
        Width = 121
        Height = 26
        MaxLength = 10
        TabOrder = 1
        Text = ''
      end
      object comboTreatment: TComboBox
        Left = 200
        Top = 26
        Width = 145
        Height = 26
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'Observation'
        Items.Strings = (
          'Observation'
          'Prediction'
          'Inactive')
      end
    end
    object pcMain: TJvPageControl
      Left = 0
      Top = 65
      Width = 499
      Height = 355
      ActivePage = tabObservationsTimes
      Align = alClient
      TabOrder = 1
      ExplicitTop = 0
      ExplicitWidth = 491
      ExplicitHeight = 217
      object tabObservationsTimes: TTabSheet
        Caption = 'Observation times and values'
        ExplicitHeight = 217
        inline frameSutraFluxObs: TframePestObs
          Left = 0
          Top = 0
          Width = 491
          Height = 322
          Align = alClient
          TabOrder = 0
          ExplicitLeft = -109
          ExplicitTop = -203
          inherited splObservations: TSplitter
            Top = 144
            Width = 491
          end
          inherited grpDirectObs: TGroupBox
            Width = 491
            Height = 144
            inherited frameObservations: TframeGrid
              Top = 20
              Width = 487
              Height = 122
              ExplicitTop = 20
              ExplicitHeight = 220
              inherited Panel: TPanel
                Top = 81
                Width = 487
                ExplicitTop = 179
                inherited lbNumber: TLabel
                  Width = 55
                  Height = 18
                  ExplicitWidth = 55
                  ExplicitHeight = 18
                end
                inherited sbAdd: TSpeedButton
                  Left = 253
                end
                inherited sbInsert: TSpeedButton
                  Left = 300
                end
                inherited sbDelete: TSpeedButton
                  Left = 347
                end
                inherited seNumber: TJvSpinEdit
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 487
                Height = 81
                ExplicitHeight = 179
              end
            end
          end
          inherited grpObsComparisons: TGroupBox
            Top = 149
            Width = 491
            inherited frameObsComparisons: TframeGrid
              Top = 20
              Width = 487
              Height = 151
              ExplicitTop = 20
              ExplicitHeight = 151
              inherited Panel: TPanel
                Top = 110
                Width = 487
                ExplicitTop = 110
                inherited lbNumber: TLabel
                  Width = 55
                  Height = 18
                  ExplicitWidth = 55
                  ExplicitHeight = 18
                end
                inherited sbAdd: TSpeedButton
                  Left = 253
                end
                inherited sbInsert: TSpeedButton
                  Left = 300
                end
                inherited sbDelete: TSpeedButton
                  Left = 347
                end
                inherited seNumber: TJvSpinEdit
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 487
                Height = 110
                ExplicitHeight = 110
              end
            end
          end
        end
      end
      object tabObjects: TTabSheet
        Caption = 'Objects'
        ImageIndex = 1
        ExplicitWidth = 483
        ExplicitHeight = 184
        DesignSize = (
          491
          322)
        object lblSrcLabel: TLabel
          Left = 14
          Top = 3
          Width = 118
          Height = 18
          Caption = 'Available objects'
        end
        object lblDstLabel: TLabel
          Left = 215
          Top = 3
          Width = 91
          Height = 18
          Caption = 'Used objects'
        end
        object lblFactor: TLabel
          Left = 15
          Top = 295
          Width = 45
          Height = 18
          Anchors = [akLeft, akBottom]
          Caption = 'Factor'
          ExplicitTop = 225
        end
        object lbSrcList: TJvListBox
          Left = 14
          Top = 23
          Width = 164
          Height = 263
          Anchors = [akLeft, akTop, akBottom]
          DragMode = dmAutomatic
          ItemHeight = 18
          Background.FillMode = bfmTile
          Background.Visible = False
          MultiSelect = True
          ParentShowHint = False
          PopupMenu = pmSelectEditAvailable
          ShowHint = True
          Sorted = True
          TabOrder = 0
          ExplicitHeight = 125
        end
        object btnIncBtn: TButton
          Left = 184
          Top = 59
          Width = 26
          Height = 26
          Hint = 'Move selected objects into "Used objects"'
          Caption = '>'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object btnIncAllBtn: TButton
          Left = 184
          Top = 91
          Width = 26
          Height = 26
          Hint = 'Move all objects into "Used objects"'
          Caption = '>>'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object btnExclBtn: TButton
          Left = 184
          Top = 124
          Width = 26
          Height = 26
          Hint = 'Move selected objects out of "Used objects"'
          Caption = '<'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
        end
        object btnExclAllBtn: TButton
          Left = 184
          Top = 156
          Width = 26
          Height = 26
          Hint = 'Move all objects out of "Used objects"'
          Caption = '<<'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
        end
        object lbDstList: TJvListBox
          Left = 216
          Top = 23
          Width = 164
          Height = 263
          Anchors = [akLeft, akTop, akBottom]
          DragMode = dmAutomatic
          ItemHeight = 18
          Background.FillMode = bfmTile
          Background.Visible = False
          MultiSelect = True
          ParentShowHint = False
          PopupMenu = pmSelectEditUsed
          ShowHint = True
          Sorted = True
          TabOrder = 1
          ExplicitHeight = 125
        end
        object edFactorFormula: TJvEdit
          Left = 61
          Top = 292
          Width = 326
          Height = 26
          Anchors = [akLeft, akRight, akBottom]
          TabOrder = 7
          Text = ''
          ExplicitTop = 154
          ExplicitWidth = 318
        end
        object btnFactorFormula: TButton
          Left = 392
          Top = 290
          Width = 90
          Height = 30
          Anchors = [akRight, akBottom]
          Caption = 'Edit F()...'
          Enabled = False
          TabOrder = 6
          ExplicitLeft = 384
          ExplicitTop = 152
        end
      end
    end
  end
  object pmSelectEditAvailable: TPopupMenu
    Left = 396
    Top = 168
    object miSelectAvailable: TMenuItem
      Caption = 'Select'
      Hint = 'Select these objects'
    end
    object miEditAvailable: TMenuItem
      Caption = 'Edit...'
      Hint = 'Edit this object in the Object Properties dialog box'
    end
    object miGotoAvailable: TMenuItem
      Caption = 'Go to'
      Hint = 'Go to the location of this object'
    end
    object miHideAvailable: TMenuItem
      Caption = 'Hide'
      Hint = 'Hide these objects'
    end
  end
  object pmSelectEditUsed: TPopupMenu
    Left = 396
    Top = 198
    object miSelectUsed: TMenuItem
      Caption = 'Select'
      Hint = 'Select these objects'
    end
    object miEditUsed: TMenuItem
      Caption = 'Edit...'
      Hint = 'Edit this object in the Object Properties dialog box'
    end
    object miGoToUsed: TMenuItem
      Caption = 'Go to'
      Hint = 'Go to the location of this object'
    end
    object miHideUsed: TMenuItem
      Caption = 'Hide'
      Hint = 'Hide these objects'
    end
  end
  object rparserThreeDFormulaElements: TRbwParser
    Left = 272
    Top = 24
  end
end
