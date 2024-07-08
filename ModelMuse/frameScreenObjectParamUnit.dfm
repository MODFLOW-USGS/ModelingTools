inherited frameScreenObjectParam: TframeScreenObjectParam
  Width = 541
  ExplicitWidth = 541
  object splitHorizontal: TSplitter [0]
    Left = 0
    Top = 89
    Width = 541
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = -239
    ExplicitTop = 129
    ExplicitWidth = 559
  end
  inherited pnlBottom: TPanel
    Width = 541
    object lblTimeSeriesInterpolation: TLabel [1]
      Left = 88
      Top = 8
      Width = 131
      Height = 15
      Caption = 'Time-series interpolation'
      Visible = False
    end
    inherited btnDelete: TBitBtn
      Left = 453
    end
    inherited btnInsert: TBitBtn
      Left = 369
    end
    object comboTimeSeriesInterpolation: TComboBox
      Left = 64
      Top = 16
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'STEPWISE'
      Visible = False
      Items.Strings = (
        'STEPWISE'
        'LINEAR'
        'LINEAR-END')
    end
  end
  inherited pnlTop: TPanel
    Width = 541
    Height = 89
    ExplicitHeight = 89
    inherited pnlCaption: TPanel
      Width = 539
      Height = 24
      Align = alTop
      ExplicitHeight = 24
    end
    object clbParameters: TJvxCheckListBox
      Left = 1
      Top = 25
      Width = 539
      Height = 63
      AllowGrayed = True
      Align = alClient
      AutoScroll = False
      Columns = 4
      ItemHeight = 15
      TabOrder = 1
      OnStateChange = clbParametersStateChange
      OnClickCheck = clbParametersClickCheck
      ExplicitWidth = 318
      InternalVersion = 202
    end
  end
  inherited pnlGrid: TPanel
    Top = 92
    Width = 541
    Height = 178
    ExplicitTop = 92
    ExplicitHeight = 178
    inherited pnlEditGrid: TPanel
      Width = 539
      inherited rdeFormula: TRbwDataEntry
        Top = 22
        ExplicitTop = 22
      end
    end
    inherited rdgModflowBoundary: TRbwDataGrid4
      Width = 539
      Height = 126
      Columns = <
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = 'F()'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 35
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
          ButtonCaption = 'F()'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 35
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
          ButtonCaption = 'F()'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 35
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
      ExplicitHeight = 126
    end
  end
end
