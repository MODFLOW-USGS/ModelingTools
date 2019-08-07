inherited frameScreenObjectParam: TframeScreenObjectParam
  object splitHorizontal: TSplitter [0]
    Left = 0
    Top = 89
    Width = 320
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = -239
    ExplicitTop = 129
    ExplicitWidth = 559
  end
  inherited pnlTop: TPanel
    Height = 89
    ExplicitHeight = 89
    inherited pnlCaption: TPanel
      Height = 24
      Align = alTop
      ExplicitHeight = 24
    end
    object clbParameters: TJvxCheckListBox
      Left = 1
      Top = 25
      Width = 318
      Height = 63
      AllowGrayed = True
      Align = alClient
      AutoScroll = False
      Columns = 4
      ItemHeight = 16
      TabOrder = 1
      OnStateChange = clbParametersStateChange
      OnClickCheck = clbParametersClickCheck
      InternalVersion = 202
    end
  end
  inherited pnlGrid: TPanel
    Top = 92
    Height = 178
    ExplicitTop = 92
    ExplicitHeight = 178
    inherited pnlEditGrid: TPanel
      inherited rdeFormula: TRbwDataEntry
        Top = 22
        ExplicitTop = 22
      end
    end
    inherited rdgModflowBoundary: TRbwDataGrid4
      Height = 126
      Columns = <
        item
          AutoAdjustRowHeights = False
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
