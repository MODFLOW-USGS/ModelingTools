object frameSwiObsInterpolated: TframeSwiObsInterpolated
  Left = 0
  Top = 0
  Width = 568
  Height = 240
  TabOrder = 0
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 568
    Height = 25
    Align = alTop
    Caption = 'SWI Observations'
    TabOrder = 0
  end
  inline frameSwiObs: TframeGrid
    Left = 0
    Top = 105
    Width = 568
    Height = 135
    Align = alClient
    TabOrder = 2
    ExplicitTop = 105
    ExplicitWidth = 568
    ExplicitHeight = 135
    inherited Panel: TPanel
      Top = 94
      Width = 568
      ExplicitTop = 94
      ExplicitWidth = 568
      inherited lbNumber: TLabel
        Width = 138
        Caption = 'Number of observation times'
        ExplicitWidth = 138
      end
      inherited sbAdd: TSpeedButton
        Left = 297
        ExplicitLeft = 297
      end
      inherited sbInsert: TSpeedButton
        Left = 351
        ExplicitLeft = 351
      end
      inherited sbDelete: TSpeedButton
        Left = 406
        ExplicitLeft = 406
      end
      inherited seNumber: TJvSpinEdit
        OnChange = frameSwiObsseNumberChange
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 568
      Height = 94
      ColCount = 6
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
      OnSelectCell = frameSwiObsGridSelectCell
      OnSetEditText = frameSwiObsGridSetEditText
      OnColSize = frameSwiObsGridColSize
      OnHorizontalScroll = frameSwiObsGridHorizontalScroll
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
          CheckMin = True
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
          ComboUsed = True
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
          WordWrapCells = True
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      ExplicitWidth = 568
      ExplicitHeight = 94
    end
  end
  object pnlMultiEdit: TPanel
    Left = 0
    Top = 25
    Width = 568
    Height = 80
    Align = alTop
    TabOrder = 1
    object lblZetaSurfaceNumber: TLabel
      Left = 0
      Top = 6
      Width = 100
      Height = 13
      Caption = 'Zeta surface number'
    end
    object lblTreatment: TLabel
      Left = 200
      Top = 6
      Width = 50
      Height = 13
      Caption = 'Treatment'
    end
    object comboMultiStatFlag: TJvImageComboBox
      Left = 91
      Top = 51
      Width = 89
      Height = 23
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      DroppedWidth = 145
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 17
      ItemIndex = -1
      TabOrder = 2
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
    object rdeZetaSurfaceNumber: TRbwDataEntry
      Left = 0
      Top = 25
      Width = 77
      Height = 21
      TabOrder = 0
      Text = '1'
      Items.Strings = (
        'False'
        'True')
      DataType = dtInteger
      Max = 2147483647.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object comboTreatment: TComboBox
      Left = 200
      Top = 25
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Observation'
      Items.Strings = (
        'Observation'
        'Prediction'
        'Inactive')
    end
  end
end
