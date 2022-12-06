object frameChemSpecies: TframeChemSpecies
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object spl1: TSplitter
    Left = 248
    Top = 0
    Width = 5
    Height = 480
    ExplicitLeft = 145
    ExplicitTop = 1
    ExplicitHeight = 115
  end
  inline frameGridImmobile: TframeGrid
    Left = 253
    Top = 0
    Width = 387
    Height = 480
    Align = alClient
    Enabled = False
    TabOrder = 0
    ExplicitLeft = 254
    ExplicitTop = 1
    ExplicitWidth = 385
    ExplicitHeight = 198
    inherited Panel: TPanel
      Top = 439
      Width = 387
      ExplicitTop = 157
      ExplicitWidth = 385
      inherited sbAdd: TSpeedButton
        Left = 199
        ExplicitLeft = 147
      end
      inherited sbInsert: TSpeedButton
        Left = 236
        ExplicitLeft = 175
      end
      inherited sbDelete: TSpeedButton
        Left = 273
        ExplicitLeft = 204
      end
      inherited seNumber: TJvSpinEdit
        Height = 28
        ExplicitHeight = 28
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 387
      Height = 439
      ColCount = 3
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
      OnSelectCell = frameGridImmobileGridSelectCell
      OnButtonClick = frameGridSpeciesGridButtonClick
      OnStateChange = frameSpeciesGridStateChange
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
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -13
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4Boolean
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
          ButtonCaption = 'Select...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -13
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = True
          ButtonWidth = 80
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
        end>
      ExplicitWidth = 387
      ExplicitHeight = 439
    end
  end
  inline frameGridMobile: TframeGrid
    Left = 0
    Top = 0
    Width = 248
    Height = 480
    Align = alLeft
    Enabled = False
    TabOrder = 1
    ExplicitLeft = 1
    ExplicitTop = 1
    ExplicitWidth = 248
    ExplicitHeight = 198
    inherited Panel: TPanel
      Top = 439
      Width = 248
      ExplicitTop = 157
      ExplicitWidth = 248
      inherited sbAdd: TSpeedButton
        Left = 123
        ExplicitLeft = 123
      end
      inherited sbInsert: TSpeedButton
        Left = 147
        ExplicitLeft = 147
      end
      inherited sbDelete: TSpeedButton
        Left = 171
        ExplicitLeft = 171
      end
      inherited seNumber: TJvSpinEdit
        Height = 28
        ExplicitHeight = 28
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 248
      Height = 439
      ColCount = 3
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
      OnSelectCell = frameGridMobileGridSelectCell
      OnButtonClick = frameGridSpeciesGridButtonClick
      OnStateChange = frameSpeciesGridStateChange
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
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -13
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4Boolean
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
          ButtonCaption = 'Select...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -13
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = True
          ButtonWidth = 80
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
        end>
      ExplicitWidth = 248
      ExplicitHeight = 439
    end
  end
  object dlgOpenSelectFile: TOpenDialog
    Filter = 'Concentration file (*.ucn)|*.ucn|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 368
    Top = 152
  end
end
