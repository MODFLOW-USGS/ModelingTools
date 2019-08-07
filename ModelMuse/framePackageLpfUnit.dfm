inherited framePackageLpf: TframePackageLpf
  Width = 463
  Height = 279
  OnResize = FrameResize
  ExplicitWidth = 463
  ExplicitHeight = 279
  DesignSize = (
    463
    279)
  inherited memoComments: TMemo
    Width = 432
    Height = 59
    ExplicitWidth = 432
    ExplicitHeight = 59
  end
  object rdgOptions: TRbwDataGrid4 [3]
    Left = 16
    Top = 127
    Width = 432
    Height = 146
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 1
    Enabled = False
    FixedCols = 0
    RowCount = 6
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 1
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
  end
end
