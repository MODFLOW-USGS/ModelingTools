inherited framePackageNpf: TframePackageNpf
  Width = 431
  Height = 329
  OnResize = FrameResize
  ExplicitWidth = 431
  ExplicitHeight = 329
  DesignSize = (
    431
    329)
  object lblInterblockMethod: TLabel [2]
    Left = 16
    Top = 119
    Width = 439
    Height = 20
    Caption = 'Method of calculating interblock transmissivity (CELL_AVERAGING)'
  end
  inherited memoComments: TMemo
    Width = 400
    Height = 51
    ExplicitWidth = 400
    ExplicitHeight = 51
  end
  object rdgOptions: TRbwDataGrid4 [4]
    Left = 16
    Top = 167
    Width = 400
    Height = 145
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 1
    Enabled = False
    FixedCols = 0
    RowCount = 10
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 2
    OnSelectCell = rdgOptionsSelectCell
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
    OnVerticalScroll = rdgOptionsVerticalScroll
    ColorSelectedRow = False
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
        Format = rcf4Boolean
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = True
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end>
    WordWrapRowCaptions = False
    ColWidths = (
      301)
  end
  object comboInterblockMethod: TJvImageComboBox [5]
    Left = 16
    Top = 138
    Width = 400
    Height = 30
    Style = csOwnerDrawVariable
    Anchors = [akLeft, akTop, akRight]
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 427
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 24
    ItemIndex = -1
    TabOrder = 1
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Harmonic mean'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Logarithmic mean'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Arithmetic and logarithmic'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Arithmetic and harmonic'
      end>
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = rdgOptions
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
