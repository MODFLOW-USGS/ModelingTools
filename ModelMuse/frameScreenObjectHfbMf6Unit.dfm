inherited frameScreenObjectHfbMf6: TframeScreenObjectHfbMf6
  inherited pnlBottom: TPanel
    Top = 144
    Height = 172
    ExplicitTop = 144
    ExplicitWidth = 541
    ExplicitHeight = 172
    DesignSize = (
      541
      172)
    object lblParameterName: TLabel [1]
      Left = 208
      Top = 46
      Width = 153
      Height = 15
      Margins.Left = 8
      Caption = 'Parameter name'
      Enabled = False
    end
    inherited btnDelete: TBitBtn
      ExplicitLeft = 453
    end
    inherited btnInsert: TBitBtn
      ExplicitLeft = 369
    end
    object rgAngleAdjustment: TRadioGroup
      Left = 0
      Top = 74
      Width = 450
      Height = 85
      Margins.Left = 8
      Margins.Right = 8
      Caption = 'Angle adjustment method'
      Enabled = False
      ItemIndex = 1
      Items.Strings = (
        'None'
        'Distribute conductivity among all sections'
        
          'Distribute conductivity among sections most nearly parallel to t' +
          'he grid')
      TabOrder = 3
    end
    object comboHfbParameters: TJvImageComboBox
      Left = 9
      Top = 43
      Width = 170
      Height = 25
      Margins.Right = 8
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      Color = clBtnFace
      DroppedWidth = 170
      Enabled = False
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 19
      ItemIndex = -1
      TabOrder = 4
      OnChange = comboHfbParametersChange
      Items = <>
    end
  end
  inherited pnlTop: TPanel
    ExplicitWidth = 541
    inherited pnlCaption: TPanel
      ExplicitWidth = 539
    end
  end
  inherited pnlGrid: TPanel
    Height = 119
    ExplicitWidth = 541
    ExplicitHeight = 119
    inherited rdgModflowBoundary: TRbwDataGrid4
      Height = 67
      ColCount = 4
      Columns = <
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
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = 'F()'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = True
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
          ButtonUsed = True
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
      ExplicitWidth = 539
      ExplicitHeight = 67
    end
  end
end
