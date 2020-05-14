inherited frmManageSutraBoundaryObservations: TfrmManageSutraBoundaryObservations
  Caption = 'frmManageSutraBoundaryObservations'
  ClientHeight = 561
  ClientWidth = 747
  OnKeyDown = FormKeyDown
  ExplicitWidth = 763
  ExplicitHeight = 600
  PixelsPerInch = 96
  TextHeight = 18
  object spltr1: TJvNetscapeSplitter
    Left = 121
    Top = 0
    Height = 520
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
    Top = 520
    Width = 747
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 420
    ExplicitWidth = 630
    DesignSize = (
      747
      41)
    object btnHelp: TBitBtn
      Left = 466
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 349
    end
    object btnCancel: TBitBtn
      Left = 652
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 4
      ExplicitLeft = 535
    end
    object btnOk: TBitBtn
      Left = 559
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnOkClick
      ExplicitLeft = 442
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
      OnClick = btnDeleteObservationClick
    end
    object btnAddObservation: TButton
      Left = 5
      Top = 6
      Width = 77
      Height = 27
      Anchors = [akLeft, akBottom]
      Caption = 'Add'
      TabOrder = 0
      OnClick = btnAddObservationClick
    end
  end
  object tvFluxObservations: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 520
    Align = alLeft
    HideSelection = False
    Indent = 20
    ReadOnly = True
    TabOrder = 1
    OnChange = tvFluxObservationsChange
    ExplicitLeft = 4
    ExplicitHeight = 420
  end
  object pnlMain: TPanel
    Left = 131
    Top = 0
    Width = 616
    Height = 520
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 499
    ExplicitHeight = 420
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 616
      Height = 65
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 499
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
        Visible = False
      end
      object edObservationName: TJvEdit
        Left = 6
        Top = 30
        Width = 121
        Height = 26
        MaxLength = 10
        TabOrder = 1
        Text = ''
        OnChange = edObservationNameChange
        OnExit = edObservationNameExit
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
        Visible = False
        Items.Strings = (
          'Observation'
          'Prediction'
          'Inactive')
      end
    end
    object pcMain: TJvPageControl
      Left = 0
      Top = 65
      Width = 616
      Height = 455
      ActivePage = tabObjects
      Align = alClient
      Enabled = False
      TabOrder = 1
      ExplicitWidth = 499
      ExplicitHeight = 355
      object tabObservationsTimes: TTabSheet
        Caption = 'Observation times and values'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 491
        ExplicitHeight = 322
        inline frameSutraFluxObs: TframePestObs
          Left = 0
          Top = 0
          Width = 608
          Height = 422
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 491
          ExplicitHeight = 322
          inherited splObservations: TSplitter
            Top = 244
            Width = 608
            ExplicitTop = 144
            ExplicitWidth = 491
          end
          inherited grpDirectObs: TGroupBox
            Width = 608
            Height = 244
            ExplicitWidth = 491
            ExplicitHeight = 144
            inherited frameObservations: TframeGrid
              Top = 20
              Width = 604
              Height = 222
              ExplicitTop = 20
              ExplicitWidth = 487
              ExplicitHeight = 122
              inherited Panel: TPanel
                Top = 181
                Width = 604
                ExplicitTop = 81
                ExplicitWidth = 487
                inherited lbNumber: TLabel
                  Width = 55
                  Height = 18
                  ExplicitWidth = 55
                  ExplicitHeight = 18
                end
                inherited sbAdd: TSpeedButton
                  Left = 316
                  ExplicitLeft = 253
                end
                inherited sbInsert: TSpeedButton
                  Left = 375
                  ExplicitLeft = 300
                end
                inherited sbDelete: TSpeedButton
                  Left = 433
                  ExplicitLeft = 347
                end
                inherited seNumber: TJvSpinEdit
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 604
                Height = 181
                ExplicitWidth = 487
                ExplicitHeight = 81
              end
            end
          end
          inherited grpObsComparisons: TGroupBox
            Top = 249
            Width = 608
            ExplicitTop = 149
            ExplicitWidth = 491
            inherited frameObsComparisons: TframeGrid
              Top = 20
              Width = 604
              Height = 151
              ExplicitTop = 20
              ExplicitWidth = 487
              ExplicitHeight = 151
              inherited Panel: TPanel
                Top = 110
                Width = 604
                ExplicitTop = 110
                ExplicitWidth = 487
                inherited lbNumber: TLabel
                  Width = 55
                  Height = 18
                  ExplicitWidth = 55
                  ExplicitHeight = 18
                end
                inherited sbAdd: TSpeedButton
                  Left = 316
                  ExplicitLeft = 253
                end
                inherited sbInsert: TSpeedButton
                  Left = 375
                  ExplicitLeft = 300
                end
                inherited sbDelete: TSpeedButton
                  Left = 433
                  ExplicitLeft = 347
                end
                inherited seNumber: TJvSpinEdit
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 604
                Height = 110
                ExplicitWidth = 487
                ExplicitHeight = 110
              end
            end
          end
        end
      end
      object tabObjects: TTabSheet
        Caption = 'Objects'
        ImageIndex = 1
        ExplicitLeft = 6
        ExplicitTop = 27
        ExplicitWidth = 491
        ExplicitHeight = 322
        DesignSize = (
          608
          422)
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
          Top = 395
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
          Height = 363
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
          OnClick = ListClick
          OnDblClick = btnIncBtnClick
          OnDragDrop = lbSrcListDragDrop
          OnDragOver = lbSrcListDragOver
          OnKeyDown = lbSrcListKeyDown
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
          OnClick = btnIncBtnClick
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
          OnClick = btnIncAllBtnClick
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
          OnClick = btnExclBtnClick
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
          OnClick = btnExclAllBtnClick
        end
        object lbDstList: TJvListBox
          Left = 216
          Top = 23
          Width = 164
          Height = 363
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
          OnClick = ListClick
          OnDblClick = btnExclBtnClick
          OnDragDrop = lbDstListDragDrop
          OnDragOver = lbDstListDragOver
          OnKeyDown = lbDstListKeyDown
        end
        object edFactorFormula: TJvEdit
          Left = 61
          Top = 392
          Width = 443
          Height = 26
          Anchors = [akLeft, akRight, akBottom]
          TabOrder = 7
          Text = ''
          OnChange = edFactorFormulaChange
        end
        object btnFactorFormula: TButton
          Left = 509
          Top = 390
          Width = 90
          Height = 30
          Anchors = [akRight, akBottom]
          Caption = 'Edit F()...'
          Enabled = False
          TabOrder = 6
          ExplicitLeft = 392
          ExplicitTop = 290
        end
      end
    end
  end
  object pmSelectEditAvailable: TPopupMenu
    OnPopup = pmSelectEditAvailablePopup
    Left = 396
    Top = 168
    object miSelectAvailable: TMenuItem
      Caption = 'Select'
      Hint = 'Select these objects'
      OnClick = miSelectAvailableClick
    end
    object miEditAvailable: TMenuItem
      Caption = 'Edit...'
      Hint = 'Edit this object in the Object Properties dialog box'
      OnClick = miEditAvailableClick
    end
    object miGotoAvailable: TMenuItem
      Caption = 'Go to'
      Hint = 'Go to the location of this object'
      OnClick = miGotoAvailableClick
    end
    object miHideAvailable: TMenuItem
      Caption = 'Hide'
      Hint = 'Hide these objects'
      OnClick = miHideAvailableClick
    end
  end
  object pmSelectEditUsed: TPopupMenu
    OnPopup = pmSelectEditUsedPopup
    Left = 396
    Top = 214
    object miSelectUsed: TMenuItem
      Caption = 'Select'
      Hint = 'Select these objects'
      OnClick = miSelectUsedClick
    end
    object miEditUsed: TMenuItem
      Caption = 'Edit...'
      Hint = 'Edit this object in the Object Properties dialog box'
      OnClick = miEditUsedClick
    end
    object miGoToUsed: TMenuItem
      Caption = 'Go to'
      Hint = 'Go to the location of this object'
      OnClick = miGoToUsedClick
    end
    object miHideUsed: TMenuItem
      Caption = 'Hide'
      Hint = 'Hide these objects'
      OnClick = miHideUsedClick
    end
  end
  object rparserThreeDFormulaElements: TRbwParser
    Left = 272
    Top = 24
  end
end
