inherited frmModflowParameters: TfrmModflowParameters
  Caption = 'Modflow Parameters'
  ClientHeight = 293
  ClientWidth = 500
  OnDestroy = FormDestroy
  ExplicitWidth = 508
  ExplicitHeight = 327
  PixelsPerInch = 96
  TextHeight = 17
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 121
    Top = 0
    Height = 252
    Align = alLeft
    MinSize = 1
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 256
    ExplicitTop = 96
    ExplicitHeight = 100
  end
  object pnlRight: TPanel
    Left = 131
    Top = 0
    Width = 369
    Height = 252
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlParameterCount: TPanel
      Left = 0
      Top = 204
      Width = 369
      Height = 48
      Align = alBottom
      TabOrder = 0
      DesignSize = (
        369
        48)
      object lblNumParameters: TLabel
        Left = 63
        Top = 14
        Width = 142
        Height = 17
        Caption = 'Number of parameters'
      end
      object btnDelete: TBitBtn
        Left = 281
        Top = 7
        Width = 82
        Height = 33
        Anchors = [akTop, akRight]
        Cancel = True
        Caption = '&Delete'
        TabOrder = 0
        OnClick = btnDeleteClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
          0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
          0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end
      object seNumberOfParameters: TJvSpinEdit
        Left = 8
        Top = 11
        Width = 49
        Height = 25
        CheckMinValue = True
        ButtonKind = bkClassic
        TabOrder = 1
        OnChange = seNumberOfParametersChange
        OnEnter = seNumberOfParametersEnter
      end
    end
    object jvplParameters: TJvPageList
      Left = 0
      Top = 0
      Width = 369
      Height = 204
      ActivePage = jvspSteadyParameters
      PropagateEnable = False
      Align = alClient
      object jvspSteadyParameters: TJvStandardPage
        Left = 0
        Top = 0
        Width = 369
        Height = 204
        Caption = 'jvspSteadyParameters'
        object pnlTop: TPanel
          Left = 0
          Top = 0
          Width = 369
          Height = 57
          Align = alTop
          TabOrder = 0
          object lblParamValue: TLabel
            Left = 24
            Top = 8
            Width = 91
            Height = 17
            Caption = 'lblParamValue'
            Enabled = False
          end
          object rdeParamValue: TRbwDataEntry
            Left = 25
            Top = 31
            Width = 33
            Height = 22
            Color = clBtnFace
            Enabled = False
            ItemHeight = 17
            TabOrder = 0
            Text = '0'
            OnChange = rdeParamValueChange
            DataType = dtReal
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
          object cbUseZone: TCheckBox
            Left = 64
            Top = 35
            Width = 97
            Height = 17
            Caption = 'cbUseZone'
            Enabled = False
            TabOrder = 1
            OnClick = cbUseZoneClick
          end
          object cbUseMultiplier: TCheckBox
            Left = 167
            Top = 35
            Width = 97
            Height = 17
            Caption = 'cbUseMultiplier'
            Enabled = False
            TabOrder = 2
            OnClick = cbUseMultiplierClick
          end
        end
        object dgSteadyParameters: TRbwDataGrid4
          Left = 0
          Top = 57
          Width = 369
          Height = 147
          Align = alClient
          ColCount = 4
          DefaultColWidth = 30
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
          TabOrder = 1
          OnMouseDown = dgSteadyParametersMouseDown
          OnMouseUp = dgSteadyParametersMouseUp
          OnSelectCell = dgSteadyParametersSelectCell
          OnSetEditText = dgSteadyParametersSetEditText
          AutoDistributeText = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnBeforeDrawCell = dgSteadyParametersBeforeDrawCell
          OnColSize = dgSteadyParametersColSize
          OnStateChange = dgSteadyParametersStateChange
          ColorRangeSelection = False
          OnHorizontalScroll = dgSteadyParametersHorizontalScroll
          ColorSelectedRow = True
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
              MaxLength = 10
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
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
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
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
              Format = rcf4Boolean
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
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
              Format = rcf4Boolean
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              AutoAdjustColWidths = True
            end>
        end
      end
      object jvspTransientParameters: TJvStandardPage
        Left = 0
        Top = 0
        Width = 369
        Height = 204
        Caption = 'jvspTransientParameters'
        object dgTransientParameters: TRbwDataGrid4
          Left = 0
          Top = 0
          Width = 369
          Height = 204
          Align = alClient
          ColCount = 2
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
          TabOrder = 0
          OnSelectCell = dgSteadyParametersSelectCell
          OnSetEditText = dgSteadyParametersSetEditText
          AutoDistributeText = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnBeforeDrawCell = dgSteadyParametersBeforeDrawCell
          ColorRangeSelection = False
          ColorSelectedRow = True
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
              MaxLength = 10
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
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
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              AutoAdjustColWidths = True
            end>
        end
      end
      object jvspBlank: TJvStandardPage
        Left = 0
        Top = 0
        Width = 369
        Height = 204
        Caption = 'jvspBlank'
        DesignSize = (
          369
          204)
        object lblBlankInstruction: TLabel
          Left = 6
          Top = 16
          Width = 357
          Height = 34
          Anchors = [akLeft, akTop, akRight]
          Caption = 
            'Select the type of parameter you wish to edit from the list on t' +
            'he left.'
          WordWrap = True
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 252
    Width = 500
    Height = 41
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      500
      41)
    object btnHelp: TBitBtn
      Left = 159
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 273
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 387
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkCancel
    end
  end
  object tvParameterTypes: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 252
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = tvParameterTypesChange
  end
  object rbwParamCountController: TRbwController
    ControlList = <
      item
        Control = seNumberOfParameters
      end
      item
        Control = btnDelete
      end
      item
        Control = lblNumParameters
      end
      item
        Control = dgSteadyParameters
      end
      item
        Control = dgTransientParameters
      end>
    Left = 320
    Top = 128
  end
end
