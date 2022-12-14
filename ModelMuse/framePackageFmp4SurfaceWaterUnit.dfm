inherited framePackageFmp4SurfaceWater: TframePackageFmp4SurfaceWater
  Height = 450
  ExplicitHeight = 450
  DesignSize = (
    422
    450)
  object cpnlgrp1: TCategoryPanelGroup [3]
    Left = 0
    Top = 157
    Width = 422
    Height = 293
    VertScrollBar.Position = 181
    VertScrollBar.Tracking = True
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Segoe UI'
    HeaderFont.Style = []
    TabOrder = 1
    object cpnlSalinityFlush: TCategoryPanel
      Top = 63
      Height = 290
      Caption = 'cpnlSalinityFlush'
      TabOrder = 0
      object rdgSalinityFlush: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 399
        Height = 264
        Align = alClient
        ColCount = 6
        Enabled = False
        FixedCols = 1
        RowCount = 7
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnSelectCell = rdgSalinityFlushSelectCell
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
          end>
        WordWrapRowCaptions = False
        ExplicitHeight = 232
      end
    end
    object cpnlOptions: TCategoryPanel
      Top = -28
      Height = 91
      Caption = 'Options'
      Enabled = False
      TabOrder = 1
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
      Top = -181
      Height = 153
      Caption = 'Print'
      TabOrder = 2
      object clbPrint: TCheckListBox
        Left = 0
        Top = 0
        Width = 399
        Height = 127
        Align = alClient
        Enabled = False
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
        Control = rdgSalinityFlush
      end>
  end
end
