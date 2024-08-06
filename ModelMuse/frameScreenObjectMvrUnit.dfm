inherited frameScreenObjectMvr: TframeScreenObjectMvr
  Height = 439
  ExplicitHeight = 439
  inherited pnlTop: TPanel [0]
    ExplicitWidth = 412
    inherited pnlCaption: TPanel
      ExplicitWidth = 410
    end
  end
  inherited pnlGrid: TPanel [1]
    Top = 304
    Height = 135
    Align = alBottom
    ExplicitTop = 304
    ExplicitWidth = 412
    ExplicitHeight = 135
    inherited pnlEditGrid: TPanel
      ExplicitWidth = 410
      object lblMvrType: TLabel [1]
        Left = 199
        Top = 5
        Width = 60
        Height = 15
        Alignment = taCenter
        Caption = 'Mover type'
      end
      object comboMvrType: TComboBox
        Left = 199
        Top = 24
        Width = 89
        Height = 23
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
          AutoAdjustCaptionRowHeights = True
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
      ExplicitWidth = 410
      ExplicitHeight = 83
      ColWidths = (
        64
        64
        64
        74)
    end
  end
  object pcMain: TPageControl [2]
    Left = 0
    Top = 25
    Width = 541
    Height = 239
    ActivePage = tabMvrMap
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 412
    object tabConnections: TTabSheet
      Caption = 'Connections'
      ImageIndex = 1
      object lblSourcePackage: TLabel
        Left = 3
        Top = 3
        Width = 83
        Height = 15
        Caption = 'Source package'
      end
      object comboSourcePackage: TComboBox
        Left = 3
        Top = 22
        Width = 306
        Height = 23
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
        Width = 533
        Height = 145
        Margins.Left = 0
        Margins.Top = 64
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        TabOrder = 1
        ExplicitTop = 64
        ExplicitWidth = 404
        ExplicitHeight = 145
        inherited Panel: TPanel
          Top = 104
          Width = 533
          ExplicitTop = 104
          ExplicitWidth = 404
          inherited lbNumber: TLabel
            Width = 107
            Caption = 'Number of receivers'
            ExplicitWidth = 107
          end
          inherited sbAdd: TSpeedButton
            Left = 308
            ExplicitLeft = 308
          end
          inherited sbInsert: TSpeedButton
            Left = 337
            OnClick = frameReceiverssbInsertClick
            ExplicitLeft = 337
          end
          inherited sbDelete: TSpeedButton
            Left = 366
            OnClick = frameReceiverssbDeleteClick
            ExplicitLeft = 366
          end
          inherited seNumber: TJvSpinEdit
            OnChange = frameReceiversseNumberChange
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 533
          Height = 104
          ColCount = 5
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
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
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = True
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
                'Nearest'
                'Nearest enclosed reach of any segment'
                'Nearest reach of any segment')
              WordWrapCaptions = True
              WordWrapCells = True
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
          ExplicitWidth = 404
          ExplicitHeight = 104
          ColWidths = (
            64
            64
            127
            64
            64)
        end
      end
    end
    object tabTime: TTabSheet
      Caption = 'Time'
    end
    object tabMvrMap: TTabSheet
      Caption = 'UZF Receiver Maps (Optional)'
      ImageIndex = 2
      object JvNetscapeSplitter1: TJvNetscapeSplitter
        Left = 110
        Top = 0
        Height = 209
        Align = alLeft
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = 264
        ExplicitTop = 112
        ExplicitHeight = 100
      end
      inline frameMapNames: TframeGrid
        Left = 0
        Top = 0
        Width = 110
        Height = 209
        Align = alLeft
        TabOrder = 0
        ExplicitWidth = 110
        ExplicitHeight = 209
        inherited Panel: TPanel
          Top = 120
          Width = 110
          Height = 89
          ExplicitTop = 120
          ExplicitWidth = 110
          ExplicitHeight = 89
          inherited lbNumber: TLabel
            Left = 10
            Top = 5
            ExplicitLeft = 10
            ExplicitTop = 5
          end
          inherited sbAdd: TSpeedButton
            Left = 3
            Top = 55
            OnClick = frameMapNamessbAddClick
            ExplicitLeft = 2
            ExplicitTop = 55
          end
          inherited sbInsert: TSpeedButton
            Left = 35
            Top = 55
            OnClick = frameMapNamessbInsertClick
            ExplicitLeft = 31
            ExplicitTop = 55
          end
          inherited sbDelete: TSpeedButton
            Left = 67
            Top = 55
            OnClick = frameMapNamessbDeleteClick
            ExplicitLeft = 60
            ExplicitTop = 55
          end
          inherited seNumber: TJvSpinEdit
            Left = 5
            Top = 26
            OnChange = frameMapNamesseNumberChange
            ExplicitLeft = 5
            ExplicitTop = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 110
          Height = 120
          OnSelectCell = frameMapNamesGridSelectCell
          OnSetEditText = frameMapNamesGridSetEditText
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = True
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
            end>
          ExplicitWidth = 110
          ExplicitHeight = 120
        end
      end
      object pnl1: TPanel
        Left = 120
        Top = 0
        Width = 413
        Height = 209
        Align = alClient
        Caption = 'pnl1'
        TabOrder = 1
        ExplicitWidth = 284
        object rdgMap: TRbwDataGrid4
          Left = 1
          Top = 1
          Width = 411
          Height = 166
          Align = alClient
          ColCount = 3
          FixedCols = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 0
          OnExit = rdgMapExit
          OnSelectCell = rdgMapSelectCell
          OnSetEditText = rdgMapSetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = False
          AutoDistributeText = False
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          ColorRangeSelection = False
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
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
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = True
              ComboUsed = False
              Format = rcf4Integer
              LimitToList = False
              Max = 100000000.000000000000000000
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
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end>
          WordWrapRowCaptions = False
          ExplicitWidth = 282
          RowHeights = (
            24
            24
            24
            24
            24)
        end
        object pnl2: TPanel
          Left = 1
          Top = 167
          Width = 411
          Height = 41
          Align = alBottom
          TabOrder = 1
          ExplicitWidth = 282
          object lblReceiverNumber: TLabel
            Left = 76
            Top = 11
            Width = 165
            Height = 15
            Caption = 'Maximum receivers per section'
          end
          object seReceiverNumber: TJvSpinEdit
            Left = 5
            Top = 6
            Width = 65
            Height = 23
            MaxValue = 1000000.000000000000000000
            Value = 1.000000000000000000
            TabOrder = 0
            OnChange = seReceiverNumberChange
          end
        end
      end
    end
  end
  inherited pnlBottom: TPanel [3]
    Top = 264
    Height = 40
    TabOrder = 3
    ExplicitTop = 264
    ExplicitWidth = 412
    ExplicitHeight = 40
    DesignSize = (
      541
      40)
    inherited btnDelete: TBitBtn
      ExplicitLeft = 324
    end
    inherited btnInsert: TBitBtn
      ExplicitLeft = 240
    end
  end
end
