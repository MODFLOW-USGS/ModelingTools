inherited frameScreenObjectCfpRechargeFraction: TframeScreenObjectCfpRechargeFraction
  Width = 529
  ExplicitWidth = 529
  inherited pnlBottom: TPanel
    Top = 216
    Width = 529
    Height = 100
    ExplicitTop = 216
    ExplicitWidth = 529
    ExplicitHeight = 100
    DesignSize = (
      529
      100)
    inherited lblNumTimes: TLabel
      Top = 65
      ExplicitTop = 65
    end
    inherited seNumberOfTimes: TJvSpinEdit
      Top = 62
      ExplicitTop = 62
    end
    inherited btnDelete: TBitBtn
      Left = 441
      Top = 57
      ExplicitLeft = 441
      ExplicitTop = 57
    end
    inherited btnInsert: TBitBtn
      Left = 357
      Top = 57
      ExplicitLeft = 357
      ExplicitTop = 57
    end
    object edCads: TLabeledEdit
      Left = 9
      Top = 28
      Width = 426
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 291
      EditLabel.Height = 15
      EditLabel.Caption = 'Conduit-Associated Drainable Storage width (W_CADS)'
      TabOrder = 3
      Text = ''
      OnChange = edCadsChange
    end
    object btnCads: TButton
      Left = 441
      Top = 19
      Width = 82
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Edit F()...'
      TabOrder = 4
    end
  end
  inherited pnlTop: TPanel
    Width = 529
    inherited pnlCaption: TPanel
      Width = 527
    end
  end
  inherited pnlGrid: TPanel
    Width = 529
    Height = 191
    inherited pnlEditGrid: TPanel
      Width = 527
    end
    inherited rdgModflowBoundary: TRbwDataGrid4
      Width = 527
      Height = 139
      ColCount = 4
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
          ButtonFont.Height = -12
          ButtonFont.Name = 'Segoe UI'
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
    end
  end
end
