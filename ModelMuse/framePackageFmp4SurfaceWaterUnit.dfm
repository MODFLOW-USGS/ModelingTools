inherited framePackageFmp4SurfaceWater: TframePackageFmp4SurfaceWater
  Height = 450
  ExplicitHeight = 450
  DesignSize = (
    422
    450)
  inherited memoComments: TMemo
    Height = 60
    ExplicitHeight = 60
  end
  object cpnlgrp1: TCategoryPanelGroup [3]
    Left = 0
    Top = 128
    Width = 422
    Height = 322
    VertScrollBar.Tracking = True
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Segoe UI'
    HeaderFont.Style = []
    TabOrder = 1
    object cpnlSurfaceWater: TCategoryPanel
      Top = 60
      Height = 318
      Caption = 'Surface Water'
      TabOrder = 0
      object rdgSurfaceWater: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 399
        Height = 292
        Align = alClient
        ColCount = 6
        Enabled = False
        FixedCols = 1
        RowCount = 9
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnSelectCell = rdgSurfaceWaterSelectCell
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
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
          end
          item
            AutoAdjustRowHeights = True
            AutoAdjustCaptionRowHeights = True
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
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'Don'#39't use'
              'Static'
              'Transient')
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            AutoAdjustCaptionRowHeights = True
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
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'Rate'
              'Volume')
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            AutoAdjustCaptionRowHeights = True
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
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            AutoAdjustCaptionRowHeights = True
            ButtonCaption = 'Open'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -12
            ButtonFont.Name = 'Segoe UI'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 50
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
            AutoAdjustCaptionRowHeights = True
            ButtonCaption = 'Open'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -12
            ButtonFont.Name = 'Segoe UI'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 50
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
          end>
        WordWrapRowCaptions = False
      end
    end
    object cpnlOptions: TCategoryPanel
      Top = 30
      Height = 30
      Caption = 'Options'
      Collapsed = True
      Enabled = False
      TabOrder = 1
      ExpandedHeight = 91
      object lblSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE: TLabel
        Left = 173
        Top = 12
        Width = 203
        Height = 15
        Caption = 'Semi-routed delivery closure tolerance'
      end
      object rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE: TRbwDataEntry
        Left = 13
        Top = 9
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbRebuild_Fully_Routed_Return: TCheckBox
        Left = 13
        Top = 37
        Width = 267
        Height = 17
        Caption = 'Rebuild fully routed return'
        Enabled = False
        TabOrder = 1
      end
    end
    object cpnlPrint: TCategoryPanel
      Top = 0
      Height = 30
      Caption = 'Print'
      Collapsed = True
      TabOrder = 2
      ExpandedHeight = 153
      object clbPrint: TCheckListBox
        Left = 0
        Top = 0
        Width = 399
        Height = 0
        Align = alClient
        Enabled = False
        ItemHeight = 15
        Items.Strings = (
          'PRINT SFR_DELIVERY'
          'PRINT SFR_DELIVERY_BY_WBS'
          'PRINT SFR_RETURN'
          'PRINT SFR_SRR_ONLY'
          'PRINT NRD'
          'PRINT NRD_BY_WBS')
        TabOrder = 0
      end
    end
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
        Control = clbPrint
      end
      item
        Control = rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE
      end
      item
        Control = cbRebuild_Fully_Routed_Return
      end
      item
        Control = rdgSurfaceWater
      end>
  end
end
