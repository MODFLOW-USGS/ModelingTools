inherited frameScreenObjectMvr: TframeScreenObjectMvr
  Height = 439
  ExplicitHeight = 439
  inherited pnlTop: TPanel [0]
  end
  inherited pnlGrid: TPanel [1]
    Top = 304
    Height = 135
    Align = alBottom
    ExplicitTop = 304
    ExplicitHeight = 135
    inherited pnlEditGrid: TPanel
      object lblMvrType: TLabel [1]
        Left = 199
        Top = 5
        Width = 55
        Height = 13
        Alignment = taCenter
        Caption = 'Mover type'
      end
      object comboMvrType: TComboBox
        Left = 199
        Top = 24
        Width = 89
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
        OnChange = comboMvrTypeChange
        Items.Strings = (
          'Factor'
          'Excess'
          'Threshold'
          'Up To')
      end
    end
    inherited rdgModflowBoundary: TRbwDataGrid4
      Height = 83
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
          LimitToList = True
          MaxLength = 0
          ParentButtonFont = False
          PickList.Strings = (
            'Factor'
            'Excess'
            'Threshold'
            'Up To')
          WordWrapCaptions = True
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      ExplicitHeight = 83
      ColWidths = (
        64
        64
        64
        64)
    end
  end
  object pcMain: TPageControl [2]
    Left = 0
    Top = 25
    Width = 320
    Height = 239
    ActivePage = tabConnections
    Align = alClient
    TabOrder = 2
    object tabConnections: TTabSheet
      Caption = 'Connections'
      ImageIndex = 1
      object lblSourcePackage: TLabel
        Left = 3
        Top = 3
        Width = 76
        Height = 13
        Caption = 'Source package'
      end
      object comboSourcePackage: TComboBox
        Left = 3
        Top = 22
        Width = 306
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = comboSourcePackageChange
        OnDropDown = comboSourcePackageDropDown
        Items.Strings = (
          'WEL, Well package'
          'DRN, Drain package'
          'RIV, River package'
          'GHB, General Head Boundary package'
          'LAK, Lake package'
          'MAW, Multiaquifer Well package'
          'SFR, Streamflow Routing package'
          'UZF6, Unsaturated Zone Flow package')
      end
      inline frameReceivers: TframeGrid
        AlignWithMargins = True
        Left = 0
        Top = 64
        Width = 312
        Height = 147
        Margins.Left = 0
        Margins.Top = 64
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        TabOrder = 1
        ExplicitTop = 64
        ExplicitWidth = 312
        ExplicitHeight = 140
        inherited Panel: TPanel
          Top = 106
          Width = 312
          ExplicitTop = 99
          ExplicitWidth = 312
          inherited lbNumber: TLabel
            Width = 97
            Caption = 'Number of receivers'
            ExplicitWidth = 97
          end
          inherited sbAdd: TSpeedButton
            Left = 158
            ExplicitLeft = 158
          end
          inherited sbInsert: TSpeedButton
            Left = 188
            OnClick = frameReceiverssbInsertClick
            ExplicitLeft = 188
          end
          inherited sbDelete: TSpeedButton
            Left = 218
            OnClick = frameReceiverssbDeleteClick
            ExplicitLeft = 218
          end
          inherited seNumber: TJvSpinEdit
            Height = 28
            OnChange = frameReceiversseNumberChange
            ExplicitHeight = 28
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 312
          Height = 106
          ColCount = 4
          OnSelectCell = frameReceiversGridSelectCell
          OnSetEditText = frameReceiversGridSetEditText
          OnBeforeDrawCell = frameReceiversGridBeforeDrawCell
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
              ComboUsed = True
              Format = rcf4String
              LimitToList = True
              MaxLength = 0
              ParentButtonFont = False
              PickList.Strings = (
                'LAK'
                'MAW'
                'SFR'
                'UZF')
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
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
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
                'First'
                'Nearest')
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
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = True
              ComboUsed = False
              Format = rcf4Integer
              LimitToList = False
              Max = 1.000000000000000000
              MaxLength = 0
              Min = 1.000000000000000000
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
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
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
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 312
          ExplicitHeight = 99
        end
      end
    end
    object tabTime: TTabSheet
      Caption = 'Time'
    end
  end
  inherited pnlBottom: TPanel [3]
    Top = 264
    Height = 40
    TabOrder = 3
    ExplicitTop = 264
    ExplicitHeight = 40
    DesignSize = (
      320
      40)
  end
end
