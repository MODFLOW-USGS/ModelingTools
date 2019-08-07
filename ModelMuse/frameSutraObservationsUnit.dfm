inherited frameSutraObservations: TframeSutraObservations
  Width = 347
  ExplicitWidth = 347
  inherited pnlBottom: TPanel
    Width = 347
    ExplicitWidth = 347
    DesignSize = (
      347
      46)
    inherited btnDelete: TBitBtn
      Left = 263
      ExplicitLeft = 263
    end
    inherited btnInsert: TBitBtn
      Left = 175
      ExplicitLeft = 175
    end
  end
  inherited pnlGrid: TPanel
    Top = 193
    Width = 347
    Height = 1
    ExplicitTop = 193
    ExplicitWidth = 347
    ExplicitHeight = 1
    inherited rdgSutraFeature: TRbwDataGrid4
      Width = 345
      Height = 84
      ColCount = 1
      Columns = <
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
        end>
      OnEndUpdate = rdgSutraFeatureEndUpdate
      ExplicitWidth = 345
      ExplicitHeight = 84
      ColWidths = (
        64)
    end
  end
  inherited pnlTop: TPanel
    Width = 347
    Height = 193
    ExplicitWidth = 347
    ExplicitHeight = 193
    DesignSize = (
      347
      193)
    inherited lblSchedule: TLabel
      Width = 94
      Caption = 'Schedule (OBSSCH)'
      ExplicitWidth = 94
    end
    object lblObservationFormat: TLabel [1]
      Left = 5
      Top = 131
      Width = 145
      Height = 13
      Caption = 'Observation format (OBSFMT)'
    end
    object lblName: TLabel [2]
      Left = 5
      Top = 30
      Width = 80
      Height = 13
      Caption = 'Name (OBSNAM)'
    end
    inherited pnlCaption: TPanel
      Width = 345
      ExplicitWidth = 345
    end
    inherited comboSchedule: TComboBox
      Top = 101
      Width = 333
      TabOrder = 2
      ExplicitTop = 101
      ExplicitWidth = 333
    end
    object comboObservationFormat: TComboBox
      Left = 5
      Top = 151
      Width = 333
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = comboObservationFormatChange
      Items.Strings = (
        'Multiple observations per line (OBS)'
        'One observation per line (OBC)')
    end
    object edName: TEdit
      Left = 5
      Top = 52
      Width = 333
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 40
      TabOrder = 1
      OnExit = edNameExit
    end
  end
end
