inherited frmManageFluxObservations: TfrmManageFluxObservations
  Left = 198
  Top = 100
  HelpType = htKeyword
  HelpKeyword = 'Manage_Flux_Observations'
  Caption = 'Manage Flow Observations'
  ClientHeight = 389
  ClientWidth = 784
  Constraints.MinHeight = 400
  Constraints.MinWidth = 550
  Position = poScreenCenter
  OnActivate = ListClick
  ExplicitWidth = 800
  ExplicitHeight = 428
  TextHeight = 18
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 121
    Top = 0
    Height = 348
    Align = alLeft
    MinSize = 1
    OnMoved = FormResize
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 119
    ExplicitTop = 1
    ExplicitHeight = 350
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 348
    Width = 784
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 349
    ExplicitWidth = 788
    DesignSize = (
      784
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
      ExplicitLeft = 503
    end
    object CancelBtn: TBitBtn
      Left = 677
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 4
      ExplicitLeft = 689
    end
    object OkBtn: TBitBtn
      Left = 584
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 3
      OnClick = OkBtnClick
      ExplicitLeft = 596
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
  object pnlMain: TPanel
    Left = 131
    Top = 0
    Width = 653
    Height = 348
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 657
    ExplicitHeight = 349
    object pcGroup: TPageControl
      Left = 0
      Top = 0
      Width = 661
      Height = 349
      ActivePage = tabObservationProperties
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 657
      object tabObservationProperties: TTabSheet
        Caption = 'Observation Properties'
        object pnlTop: TPanel
          Left = 0
          Top = 0
          Width = 653
          Height = 65
          Align = alTop
          TabOrder = 0
          ExplicitWidth = 649
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
            OnChange = comboTreatmentChange
            Items.Strings = (
              'Observation'
              'Prediction'
              'Inactive')
          end
        end
        object pcMain: TJvPageControl
          Left = 0
          Top = 65
          Width = 645
          Height = 250
          ActivePage = tabObjects
          Align = alClient
          TabOrder = 1
          ExplicitWidth = 649
          ExplicitHeight = 251
          object tabObservationsTimes: TTabSheet
            Caption = 'Observation times and values'
            OnResize = tabObservationsTimesResize
            DesignSize = (
              637
              217)
            object lblNumObsTimes: TLabel
              Left = 63
              Top = 190
              Width = 114
              Height = 18
              Anchors = [akLeft, akBottom]
              Caption = 'Number of times'
              ExplicitTop = 225
            end
            object rdgFluxObsTimes: TRbwDataGrid4
              AlignWithMargins = True
              Left = 3
              Top = 32
              Width = 631
              Height = 145
              Margins.Top = 32
              Margins.Bottom = 40
              Align = alClient
              ColCount = 6
              FixedCols = 1
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              TabOrder = 2
              OnExit = rdgFluxObsTimesExit
              OnSelectCell = rdgFluxObsTimesSelectCell
              ExtendedAutoDistributeText = False
              AutoMultiEdit = True
              AutoDistributeText = True
              AutoIncreaseColCount = False
              AutoIncreaseRowCount = True
              SelectedRowOrColumnColor = clAqua
              UnselectableColor = clBtnFace
              OnBeforeDrawCell = rdgFluxObsTimesBeforeDrawCell
              OnColSize = rdgFluxObsTimesColSize
              OnEndUpdate = rdgFluxObsTimesEndUpdate
              ColorRangeSelection = False
              OnHorizontalScroll = rdgFluxObsTimesHorizontalScroll
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
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
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
                  WordWrapCaptions = True
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
                  AutoAdjustColWidths = True
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
                    'Variance (0)'
                    'Standard dev. (1)'
                    'Coef. of var. (2)'
                    'Weight (3)'
                    'Sq. rt. of weight (4)')
                  WordWrapCaptions = False
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
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
                  WordWrapCells = True
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = False
                end>
              WordWrapRowCaptions = False
              ExplicitWidth = 639
              ExplicitHeight = 147
              ColWidths = (
                64
                64
                64
                64
                120
                145)
              RowHeights = (
                24
                24)
            end
            object seNumObsTimes: TJvSpinEdit
              Left = 0
              Top = 188
              Width = 57
              Height = 26
              ButtonKind = bkClassic
              Anchors = [akLeft, akBottom]
              TabOrder = 5
              OnChange = seNumObsTimesChange
              ExplicitTop = 190
            end
            object btnDelete: TButton
              Left = 443
              Top = 187
              Width = 87
              Height = 27
              Anchors = [akRight, akBottom]
              Caption = 'Delete'
              TabOrder = 3
              OnClick = btnDeleteClick
              ExplicitLeft = 463
              ExplicitTop = 189
            end
            object btnInsert: TButton
              Left = 538
              Top = 187
              Width = 87
              Height = 27
              Anchors = [akRight, akBottom]
              Caption = 'Insert'
              TabOrder = 4
              OnClick = btnInsertClick
              ExplicitLeft = 558
              ExplicitTop = 189
            end
            object rdeMultiValueEdit: TRbwDataEntry
              Left = 80
              Top = 3
              Width = 61
              Height = 22
              TabOrder = 0
              Text = '0'
              OnChange = rdeMultiValueEditChange
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object comboMultiStatFlag: TJvImageComboBox
              Left = 267
              Top = 3
              Width = 89
              Height = 28
              Style = csOwnerDrawVariable
              ButtonStyle = fsLighter
              DroppedWidth = 145
              ImageHeight = 0
              ImageWidth = 0
              ItemHeight = 22
              ItemIndex = -1
              TabOrder = 1
              OnChange = comboMultiStatFlagChange
              Items = <
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Variance'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Standard dev.'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Coef. of var.'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Weight'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Sq. rt. of weight'
                end>
            end
          end
          object tabMassFlux: TTabSheet
            Caption = 'Mass flux observation times and values'
            ImageIndex = 2
            TabVisible = False
            DesignSize = (
              637
              217)
            object lblNumMt3dmsObsTimes: TLabel
              Left = 63
              Top = 190
              Width = 114
              Height = 18
              Anchors = [akLeft, akBottom]
              Caption = 'Number of times'
              ExplicitTop = 225
            end
            object rdeMassFluxMultiValueEdit: TRbwDataEntry
              Left = 80
              Top = 3
              Width = 61
              Height = 22
              TabOrder = 0
              Text = '0'
              OnChange = rdeMassFluxMultiValueEditChange
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object comboMt3dmsSpecies: TJvImageComboBox
              Left = 267
              Top = 3
              Width = 89
              Height = 28
              Style = csOwnerDrawVariable
              ButtonStyle = fsLighter
              DroppedWidth = 145
              ImageHeight = 0
              ImageWidth = 0
              ItemHeight = 22
              ItemIndex = -1
              TabOrder = 1
              OnChange = comboMt3dmsSpeciesChange
              Items = <
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Variance'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Standard dev.'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Coef. of var.'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Weight'
                end
                item
                  Brush.Style = bsClear
                  Indent = 0
                  Text = 'Sq. rt. of weight'
                end>
            end
            object rdgConcFluxObsTimes: TRbwDataGrid4
              AlignWithMargins = True
              Left = 3
              Top = 32
              Width = 631
              Height = 145
              Margins.Top = 32
              Margins.Bottom = 40
              Align = alClient
              ColCount = 7
              DefaultColWidth = 40
              FixedCols = 1
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              TabOrder = 2
              OnExit = rdgConcFluxObsTimesExit
              OnSelectCell = rdgConcFluxObsTimesSelectCell
              OnSetEditText = rdgConcFluxObsTimesSetEditText
              ExtendedAutoDistributeText = False
              AutoMultiEdit = True
              AutoDistributeText = True
              AutoIncreaseColCount = False
              AutoIncreaseRowCount = True
              SelectedRowOrColumnColor = clAqua
              UnselectableColor = clBtnFace
              OnBeforeDrawCell = rdgConcFluxObsTimesBeforeDrawCell
              OnColSize = rdgConcFluxObsTimesColSize
              OnEndUpdate = rdgConcFluxObsTimesEndUpdate
              ColorRangeSelection = False
              OnHorizontalScroll = rdgConcFluxObsTimesHorizontalScroll
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
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
                    'Observation time'
                    'Observation frequency')
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
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
                  WordWrapCaptions = True
                  WordWrapCells = True
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = False
                end>
              WordWrapRowCaptions = False
              ExplicitWidth = 639
              ExplicitHeight = 147
              ColWidths = (
                40
                40
                40
                40
                40
                40
                215)
            end
            object seNumMt3dmsObsTimes: TJvSpinEdit
              Left = 0
              Top = 188
              Width = 57
              Height = 26
              ButtonKind = bkClassic
              Anchors = [akLeft, akBottom]
              TabOrder = 5
              OnChange = seNumMt3dmsObsTimesChange
              ExplicitTop = 190
            end
            object btnDeleteMt3dmsFlux: TButton
              Left = 443
              Top = 187
              Width = 87
              Height = 27
              Anchors = [akRight, akBottom]
              Caption = 'Delete'
              TabOrder = 3
              OnClick = btnDeleteMt3dmsFluxClick
              ExplicitLeft = 463
              ExplicitTop = 189
            end
            object btnInsertMt3dmsFlux: TButton
              Left = 538
              Top = 187
              Width = 87
              Height = 27
              Anchors = [akRight, akBottom]
              Caption = 'Insert'
              TabOrder = 4
              OnClick = btnInsertMt3dmsFluxClick
              ExplicitLeft = 558
              ExplicitTop = 189
            end
          end
          object tabObjects: TTabSheet
            Caption = 'Objects'
            ImageIndex = 1
            DesignSize = (
              637
              217)
            object SrcLabel: TLabel
              Left = 14
              Top = 3
              Width = 118
              Height = 18
              Caption = 'Available objects'
            end
            object DstLabel: TLabel
              Left = 215
              Top = 3
              Width = 91
              Height = 18
              Caption = 'Used objects'
            end
            object lblFactor: TLabel
              Left = 15
              Top = 190
              Width = 45
              Height = 18
              Anchors = [akLeft, akBottom]
              Caption = 'Factor'
              ExplicitTop = 225
            end
            object SrcList: TJvListBox
              Left = 14
              Top = 23
              Width = 164
              Height = 158
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
              OnDblClick = IncBtnClick
              OnDragDrop = SrcListDragDrop
              OnDragOver = SrcListDragOver
              OnKeyDown = SrcListKeyDown
            end
            object IncBtn: TButton
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
              OnClick = IncBtnClick
            end
            object IncAllBtn: TButton
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
              OnClick = IncAllBtnClick
            end
            object ExclBtn: TButton
              Left = 184
              Top = 123
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
              OnClick = ExclBtnClick
            end
            object ExclAllBtn: TButton
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
              OnClick = ExclAllBtnClick
            end
            object DstList: TJvListBox
              Left = 216
              Top = 23
              Width = 164
              Height = 158
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
              OnDblClick = ExclBtnClick
              OnDragDrop = DstListDragDrop
              OnDragOver = DstListDragOver
              OnKeyDown = DstListKeyDown
            end
            object edFactorFormula: TJvEdit
              Left = 61
              Top = 187
              Width = 460
              Height = 26
              Anchors = [akLeft, akRight, akBottom]
              TabOrder = 7
              Text = ''
              OnChange = edFactorFormulaChange
              OnExit = edFactorFormulaExit
              ExplicitTop = 188
              ExplicitWidth = 468
            end
            object btnFactorFormula: TButton
              Left = 526
              Top = 185
              Width = 90
              Height = 30
              Anchors = [akRight, akBottom]
              Caption = 'Edit F()...'
              Enabled = False
              TabOrder = 6
              OnClick = btnFactorFormulaClick
              ExplicitLeft = 538
              ExplicitTop = 186
            end
          end
        end
      end
      object tabObservationGroupNames: TTabSheet
        Caption = 'Observation Group Names'
        ImageIndex = 1
        object rdgGroupNames: TRbwDataGrid4
          Left = 0
          Top = 0
          Width = 653
          Height = 317
          Align = alClient
          ColCount = 2
          FixedCols = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 0
          OnSetEditText = rdgGroupNamesSetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = False
          AutoDistributeText = True
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
              AutoAdjustColWidths = True
            end>
          WordWrapRowCaptions = False
        end
      end
    end
  end
  object tvFluxObservations: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 348
    Align = alLeft
    HideSelection = False
    Indent = 20
    ReadOnly = True
    TabOrder = 0
    OnChange = tvFluxObservationsChange
  end
  object rparserThreeDFormulaElements: TRbwParser
    Left = 272
    Top = 24
  end
  object pmSelectEditAvailable: TPopupMenu
    OnPopup = pmSelectEditAvailablePopup
    Left = 568
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
    Left = 568
    Top = 232
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
end
