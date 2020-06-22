inherited frameScreenObjectStr: TframeScreenObjectStr
  Height = 355
  ExplicitHeight = 355
  inherited splitHorizontal: TSplitter
    Top = 121
    ExplicitTop = 121
  end
  inherited pnlBottom: TPanel
    Top = 274
    ExplicitTop = 274
    inherited comboFormulaInterp: TComboBox
      Left = 206
      Top = 45
      OnChange = comboFormulaInterpChange
      ExplicitLeft = 206
      ExplicitTop = 45
    end
  end
  inherited pnlTop: TPanel
    Height = 121
    ExplicitHeight = 121
    inherited clbParameters: TJvxCheckListBox
      Top = 66
      Height = 54
      AllowGrayed = False
      CheckKind = ckRadioButtons
      TabOrder = 2
      OnClick = clbParametersClick
      ExplicitTop = 66
      ExplicitHeight = 54
    end
    object pnlNumber: TPanel
      Left = 1
      Top = 25
      Width = 318
      Height = 41
      Align = alTop
      TabOrder = 1
      object lblSegmentNumber: TLabel
        Left = 134
        Top = 17
        Width = 110
        Height = 13
        Caption = 'Segment number (Seg)'
      end
      object seSegmentNumber: TJvSpinEdit
        Left = 7
        Top = 14
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 0
        OnChange = seSegmentNumberChange
      end
    end
  end
  inherited pnlGrid: TPanel
    Top = 124
    Height = 150
    ExplicitTop = 124
    ExplicitHeight = 150
    inherited pnlEditGrid: TPanel
      Height = 56
      ExplicitHeight = 56
    end
    inherited rdgModflowBoundary: TRbwDataGrid4
      Top = 57
      Height = 92
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
          ComboUsed = True
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
      ExplicitTop = 57
      ExplicitHeight = 92
    end
  end
end
