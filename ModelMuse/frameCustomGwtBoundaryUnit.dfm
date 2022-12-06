inherited frameCustomGwtBoundary: TframeCustomGwtBoundary
  Width = 377
  ExplicitWidth = 377
  inherited pnlBottom: TPanel
    Top = 248
    Width = 377
    Height = 68
    ExplicitTop = 248
    ExplicitWidth = 377
    ExplicitHeight = 68
    DesignSize = (
      377
      68)
    object lblChemSpecies: TLabel [1]
      Left = 165
      Top = 39
      Width = 91
      Height = 15
      Caption = 'Chemical species'
    end
    inherited btnDelete: TBitBtn
      Left = 289
      ExplicitLeft = 289
    end
    inherited btnInsert: TBitBtn
      Left = 205
      ExplicitLeft = 205
    end
    object comboChemSpecies: TComboBox
      Left = 14
      Top = 36
      Width = 145
      Height = 23
      Style = csDropDownList
      TabOrder = 3
    end
  end
  inherited pnlTop: TPanel
    Width = 377
    ExplicitWidth = 377
    inherited pnlCaption: TPanel
      Width = 375
      ExplicitWidth = 375
    end
  end
  inherited pnlGrid: TPanel
    Width = 377
    Height = 223
    ExplicitWidth = 377
    ExplicitHeight = 223
    inherited pnlEditGrid: TPanel
      Width = 375
      ExplicitWidth = 375
    end
    inherited rdgModflowBoundary: TRbwDataGrid4
      Width = 375
      Height = 171
      RowCount = 4
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
        end>
      ExplicitWidth = 375
      ExplicitHeight = 171
    end
  end
end
