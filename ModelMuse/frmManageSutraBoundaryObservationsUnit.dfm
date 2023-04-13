inherited frmManageSutraBoundaryObservations: TfrmManageSutraBoundaryObservations
  HelpType = htKeyword
  HelpKeyword = 'Manage_SUTRA_Boundary_Observati'
  Caption = 'Manage SUTRA Boundary Observations'
  ClientHeight = 560
  ClientWidth = 780
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  ExplicitWidth = 796
  ExplicitHeight = 599
  TextHeight = 18
  object spltr1: TJvNetscapeSplitter
    Left = 165
    Top = 0
    Height = 519
    Align = alLeft
    MinSize = 1
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 121
    ExplicitTop = -124
    ExplicitHeight = 350
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 519
    Width = 780
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      780
      41)
    object btnHelp: TBitBtn
      Left = 491
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnHelpClick
      ExplicitLeft = 495
    end
    object btnCancel: TBitBtn
      Left = 677
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 4
      ExplicitLeft = 681
    end
    object btnOk: TBitBtn
      Left = 584
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnOkClick
      ExplicitLeft = 588
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
    Width = 165
    Height = 519
    Align = alLeft
    HideSelection = False
    Indent = 20
    ReadOnly = True
    TabOrder = 1
    OnChange = tvFluxObservationsChange
  end
  object pnlMain: TPanel
    Left = 175
    Top = 0
    Width = 605
    Height = 519
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 613
      Height = 65
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 605
      object lblObservationName: TLabel
        Left = 6
        Top = 6
        Width = 195
        Height = 18
        Caption = 'Observation series identifier'
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
        Top = 30
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
      Width = 605
      Height = 454
      ActivePage = tabObservationsTimes
      Align = alClient
      Enabled = False
      TabOrder = 1
      object tabObservationsTimes: TTabSheet
        Caption = 'Observation times and values'
        inline frameSutraFluxObs: TframePestObs
          Left = 0
          Top = 0
          Width = 601
          Height = 422
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 597
          ExplicitHeight = 421
          inherited splObservations: TSplitter
            Top = 245
            Width = 601
            ExplicitLeft = 2
            ExplicitTop = 248
            ExplicitWidth = 601
          end
          inherited grpDirectObs: TGroupBox
            Width = 601
            Height = 245
            ExplicitWidth = 597
            ExplicitHeight = 244
            inherited frameObservations: TframeGrid
              Top = 20
              Width = 597
              Height = 223
              ExplicitTop = 20
              ExplicitWidth = 593
              ExplicitHeight = 222
              inherited Panel: TPanel
                Top = 191
                Width = 597
                ExplicitTop = 190
                ExplicitWidth = 593
                inherited lbNumber: TLabel
                  Width = 208
                  Height = 18
                  ExplicitWidth = 208
                  ExplicitHeight = 18
                end
                inherited seNumber: TJvSpinEdit
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 597
                Height = 191
                ExplicitWidth = 593
                ExplicitHeight = 190
              end
            end
          end
          inherited grpObsComparisons: TGroupBox
            Top = 250
            Width = 601
            ExplicitTop = 249
            ExplicitWidth = 597
            inherited frameObsComparisons: TframeGrid
              Top = 20
              Width = 597
              Height = 150
              ExplicitTop = 20
              ExplicitWidth = 593
              ExplicitHeight = 150
              inherited Panel: TPanel
                Top = 115
                Width = 597
                ExplicitTop = 115
                ExplicitWidth = 593
                inherited lbNumber: TLabel
                  Width = 251
                  Height = 18
                  ExplicitWidth = 251
                  ExplicitHeight = 18
                end
                inherited seNumber: TJvSpinEdit
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 597
                Height = 115
                ExplicitWidth = 593
                ExplicitHeight = 115
              end
            end
          end
        end
      end
      object tabObjects: TTabSheet
        Caption = 'Objects'
        ImageIndex = 1
        OnResize = FormResize
        DesignSize = (
          597
          421)
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
          Top = 394
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
          Height = 362
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
          Height = 362
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
          Top = 391
          Width = 424
          Height = 26
          Anchors = [akLeft, akRight, akBottom]
          TabOrder = 7
          Text = ''
          OnChange = edFactorFormulaChange
          ExplicitTop = 392
          ExplicitWidth = 432
        end
        object btnFactorFormula: TButton
          Left = 490
          Top = 389
          Width = 90
          Height = 30
          Anchors = [akRight, akBottom]
          Caption = 'Edit F()...'
          Enabled = False
          TabOrder = 6
          OnClick = btnFactorFormulaClick
          ExplicitLeft = 502
          ExplicitTop = 390
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
