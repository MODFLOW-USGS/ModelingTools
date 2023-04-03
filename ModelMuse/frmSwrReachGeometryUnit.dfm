inherited frmSwrReachGeometry: TfrmSwrReachGeometry
  HelpType = htKeyword
  HelpKeyword = 'SWR_Reach_Geometry_Dialog_Box'
  Caption = 'SWR Reach Geometry'
  ClientHeight = 555
  ClientWidth = 782
  ExplicitWidth = 794
  ExplicitHeight = 593
  TextHeight = 18
  object splttrMain: TJvNetscapeSplitter
    Left = 0
    Top = 353
    Width = 782
    Height = 10
    Cursor = crVSplit
    Align = alBottom
    MinSize = 1
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitTop = 0
    ExplicitWidth = 139
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 513
    Width = 782
    Height = 42
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 512
    ExplicitWidth = 778
    DesignSize = (
      782
      42)
    object btnHelp: TBitBtn
      Left = 507
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 503
    end
    object btnOK: TBitBtn
      Left = 596
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 592
    end
    object btnCancel: TBitBtn
      Left = 685
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 681
    end
  end
  object jvplGeometry: TJvPageList
    Left = 0
    Top = 363
    Width = 782
    Height = 150
    ActivePage = jvspTable
    PropagateEnable = False
    Align = alBottom
    ExplicitTop = 362
    ExplicitWidth = 778
    object jvspCrossSection: TJvStandardPage
      Left = 0
      Top = 0
      Width = 782
      Height = 150
      Caption = 'jvspCrossSection'
      inline frameCrossSection: TframePlotGrid
        Left = 0
        Top = 0
        Width = 786
        Height = 150
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 782
        ExplicitHeight = 150
        inherited pbPlot: TPaintBox
          Left = 326
          Width = 460
          Height = 109
          OnPaint = frameCrossSectionpbPlotPaint
          ExplicitLeft = 326
          ExplicitWidth = 456
          ExplicitHeight = 109
        end
        inherited splPlot: TSplitter
          Left = 321
          Height = 109
          ExplicitLeft = 321
          ExplicitHeight = 109
        end
        inherited Panel: TPanel
          Top = 109
          Width = 786
          ExplicitTop = 109
          ExplicitWidth = 782
          inherited lbNumber: TLabel
            Width = 146
            Height = 18
            Caption = 'Number (NGEOPTS)'
            ExplicitWidth = 146
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 231
            OnClick = frameCrossSectionsbAddClick
            ExplicitLeft = 231
          end
          inherited sbInsert: TSpeedButton
            Left = 260
            OnClick = frameCrossSectionsbInsertClick
            ExplicitLeft = 260
          end
          inherited sbDelete: TSpeedButton
            Left = 289
            OnClick = frameCrossSectionsbDeleteClick
            ExplicitLeft = 289
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            OnChange = frameCrossSectionseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 321
          Height = 109
          ColCount = 2
          OnSetEditText = frameCrossSectionGridSetEditText
          OnEndUpdate = frameCrossSectionGridEndUpdate
          Columns = <
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
              ButtonFont.Height = -13
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
            end>
          ExplicitWidth = 321
          ExplicitHeight = 109
        end
      end
    end
    object jvspTable: TJvStandardPage
      Left = 0
      Top = 0
      Width = 782
      Height = 150
      Caption = 'jvspTable'
      ExplicitWidth = 778
      inline frameTable: TframePlotGrid
        Left = 0
        Top = 0
        Width = 782
        Height = 150
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 778
        ExplicitHeight = 150
        inherited pbPlot: TPaintBox
          Left = 454
          Width = 328
          Height = 109
          OnPaint = frameTablepbPlotPaint
          ExplicitLeft = 454
          ExplicitWidth = 328
          ExplicitHeight = 109
        end
        inherited splPlot: TSplitter
          Left = 449
          Height = 109
          ExplicitLeft = 449
          ExplicitHeight = 109
        end
        inherited Panel: TPanel
          Top = 109
          Width = 782
          ExplicitTop = 109
          ExplicitWidth = 778
          inherited lbNumber: TLabel
            Width = 146
            Height = 18
            Caption = 'Number (NGEOPTS)'
            ExplicitWidth = 146
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 230
            OnClick = frameTablesbAddClick
            ExplicitLeft = 231
          end
          inherited sbInsert: TSpeedButton
            Left = 258
            OnClick = frameTablesbInsertClick
            ExplicitLeft = 260
          end
          inherited sbDelete: TSpeedButton
            Left = 288
            OnClick = frameTablesbDeleteClick
            ExplicitLeft = 289
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            OnChange = frameTableseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 449
          Height = 109
          ColCount = 5
          OnSelectCell = frameTableGridSelectCell
          OnSetEditText = frameTableGridSetEditText
          OnEndUpdate = frameTableGridEndUpdate
          Columns = <
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
              ButtonFont.Height = -13
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
              ButtonFont.Height = -13
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
              ButtonFont.Height = -13
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
              ButtonFont.Height = -13
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
            end>
          ExplicitWidth = 449
          ExplicitHeight = 109
        end
      end
    end
    object jvspBlank: TJvStandardPage
      Left = 0
      Top = 0
      Width = 782
      Height = 150
    end
  end
  inline frameGeometry: TframeReachGeomGrid
    Left = 0
    Top = 0
    Width = 782
    Height = 353
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 778
    ExplicitHeight = 352
    inherited Panel: TPanel
      Top = 312
      Width = 782
      ExplicitTop = 311
      ExplicitWidth = 778
      inherited lbNumber: TLabel
        Width = 55
        Height = 18
        ExplicitWidth = 55
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 230
        Top = 3
        OnClick = frameGeometrysbAddClick
        ExplicitLeft = 231
        ExplicitTop = 3
      end
      inherited sbInsert: TSpeedButton
        Left = 258
        Top = 3
        OnClick = frameGeometrysbInsertClick
        ExplicitLeft = 260
        ExplicitTop = 3
      end
      inherited sbDelete: TSpeedButton
        Left = 288
        Top = 3
        OnClick = frameGeometrysbDeleteClick
        ExplicitLeft = 289
        ExplicitTop = 3
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        OnChange = frameGeometryseNumberChange
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 776
      Height = 269
      FixedCols = 0
      OnRowMoved = frameGeometryGridRowMoved
      OnSelectCell = frameGeometryGridSelectCell
      OnSetEditText = frameGeometryGridSetEditText
      OnBeforeDrawCell = frameGeometryGridBeforeDrawCell
      OnEndUpdate = frameGeometryGridEndUpdate
      ExplicitTop = 40
      ExplicitWidth = 772
      ExplicitHeight = 268
    end
    inherited comboType: TJvImageComboBox
      Height = 28
      ItemHeight = 22
      ExplicitHeight = 28
    end
    inherited comboMethod: TJvImageComboBox
      Height = 28
      ItemHeight = 22
      ExplicitHeight = 28
    end
  end
end
