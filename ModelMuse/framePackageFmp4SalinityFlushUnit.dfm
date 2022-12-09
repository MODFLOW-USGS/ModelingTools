inherited framePackageFmp4SalinityFlush: TframePackageFmp4SalinityFlush
  Height = 507
  ExplicitHeight = 507
  DesignSize = (
    422
    507)
  object cpnlgrp1: TCategoryPanelGroup [3]
    Left = 0
    Top = 157
    Width = 422
    VertScrollBar.Tracking = True
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Segoe UI'
    HeaderFont.Style = []
    TabOrder = 1
    object cpnlPrint: TCategoryPanel
      Top = 0
      Height = 30
      Caption = 'Print'
      Collapsed = True
      TabOrder = 0
      ExpandedHeight = 89
      object clbPrint: TCheckListBox
        Left = 0
        Top = 0
        Width = 416
        Height = 0
        Align = alClient
        Enabled = False
        ItemHeight = 15
        Items.Strings = (
          'PRINT BYFARM'
          'PRINT BYFARM_BYCROP'
          'PRINT ALL')
        TabOrder = 0
      end
    end
    object cpnlOptions: TCategoryPanel
      Top = 30
      Height = 313
      Caption = 'Options'
      TabOrder = 1
      object pnl1: TPanel
        Left = 0
        Top = 0
        Width = 416
        Height = 65
        Align = alTop
        TabOrder = 0
        object lblExpressionLength: TLabel
          Left = 140
          Top = 11
          Width = 93
          Height = 15
          Caption = 'Expression length'
        end
        object lblExpressionMin: TLabel
          Left = 140
          Top = 40
          Width = 186
          Height = 15
          Caption = 'EXPRESSION_VARIABLE_NEARZERO'
        end
        object seExpressionLength: TJvSpinEdit
          Left = 13
          Top = 8
          Width = 121
          Height = 23
          CheckMaxValue = False
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          Enabled = False
          TabOrder = 0
        end
        object rdeExpressionMin: TRbwDataEntry
          Left = 13
          Top = 37
          Width = 121
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 1
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
      end
      object rdgSalinityFlush: TRbwDataGrid4
        Left = 0
        Top = 65
        Width = 416
        Height = 222
        Align = alClient
        ColCount = 3
        Enabled = False
        FixedCols = 1
        RowCount = 8
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 1
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
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'don'#39't use'
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
              'Array'
              'List')
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
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
        Control = seExpressionLength
      end
      item
        Control = rdeExpressionMin
      end
      item
        Control = rdgSalinityFlush
      end>
  end
end
