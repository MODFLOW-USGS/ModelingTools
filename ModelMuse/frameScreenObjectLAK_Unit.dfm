inherited frameScreenObjectLAK: TframeScreenObjectLAK
  Width = 463
  Height = 567
  ExplicitWidth = 463
  ExplicitHeight = 567
  inherited pnlBottom: TPanel
    Top = 448
    Width = 463
    Height = 119
    ExplicitTop = 448
    ExplicitWidth = 463
    ExplicitHeight = 119
    DesignSize = (
      463
      119)
    object lblInitialStage: TLabel [1]
      Left = 79
      Top = 80
      Width = 56
      Height = 13
      Caption = 'Initial stage'
    end
    object lblCenterLake: TLabel [2]
      Left = 362
      Top = 48
      Width = 55
      Height = 13
      Caption = 'Center lake'
    end
    object lblSill: TLabel [3]
      Left = 362
      Top = 80
      Width = 12
      Height = 13
      Caption = 'Sill'
    end
    object lblLakeID: TLabel [4]
      Left = 79
      Top = 48
      Width = 36
      Height = 13
      Caption = 'Lake ID'
    end
    inherited btnDelete: TBitBtn
      Left = 375
      ExplicitLeft = 375
    end
    inherited btnInsert: TBitBtn
      Left = 287
      ExplicitLeft = 287
    end
    object rdeInitialStage: TRbwDataEntry
      Left = 8
      Top = 77
      Width = 65
      Height = 22
      TabOrder = 6
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object rdeCenterLake: TRbwDataEntry
      Left = 291
      Top = 45
      Width = 65
      Height = 22
      TabOrder = 4
      Text = '0'
      OnChange = rdeCenterLakeChange
      DataType = dtInteger
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object rdeSill: TRbwDataEntry
      Left = 291
      Top = 73
      Width = 65
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 5
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object rdeLakeID: TRbwDataEntry
      Left = 8
      Top = 45
      Width = 65
      Height = 22
      TabOrder = 3
      Text = '1'
      DataType = dtInteger
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
  inherited pnlTop: TPanel
    Width = 463
    ExplicitWidth = 463
    inherited pnlCaption: TPanel
      Width = 461
      ExplicitWidth = 461
    end
  end
  inherited pnlGrid: TPanel
    Width = 463
    Height = 96
    Align = alTop
    ExplicitWidth = 463
    ExplicitHeight = 96
    inherited pnlEditGrid: TPanel
      Width = 461
      ExplicitWidth = 461
    end
    inherited rdgModflowBoundary: TRbwDataGrid4
      Width = 461
      Height = 44
      ColCount = 8
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
        end
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end>
      ExplicitLeft = 1
      ExplicitTop = 51
      ExplicitWidth = 461
      ExplicitHeight = 44
      ColWidths = (
        64
        64
        64
        64
        64
        64
        64
        64)
    end
  end
  object pcLake: TPageControl
    Left = 0
    Top = 121
    Width = 463
    Height = 327
    ActivePage = tabGage
    Align = alClient
    TabOrder = 3
    object tabLakeProperties: TTabSheet
      Caption = 'Lake Properties'
    end
    object tabGage: TTabSheet
      Caption = 'Gage'
      ImageIndex = 3
      DesignSize = (
        455
        299)
      object gbGage: TGroupBox
        Left = 3
        Top = 17
        Width = 449
        Height = 160
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Gage output'
        TabOrder = 0
        DesignSize = (
          449
          160)
        object cbGagStandard: TCheckBox
          Left = 3
          Top = 24
          Width = 430
          Height = 17
          AllowGrayed = True
          Caption = 'Time, stage, volume, and concentration'
          TabOrder = 0
          OnClick = cbGagStandardClick
        end
        object cbGagFluxAndCond: TCheckBox
          Left = 3
          Top = 47
          Width = 443
          Height = 17
          AllowGrayed = True
          Caption = 'Time-step fluxes for lake and total lake conductance'
          Enabled = False
          TabOrder = 1
          OnClick = cbGagFluxAndCondClick
        end
        object cbGagDelta: TCheckBox
          Left = 3
          Top = 70
          Width = 443
          Height = 17
          AllowGrayed = True
          Caption = 'Changes in stage, volume, and concentration for lake'
          Enabled = False
          TabOrder = 2
          OnClick = cbGagDeltaClick
        end
        object cbGage4: TCheckBox
          Left = 3
          Top = 93
          Width = 443
          Height = 56
          AllowGrayed = True
          Anchors = [akLeft, akTop, akRight]
          Caption = 
            'Time, lake stage, lake volume, solute concentration, rate of cha' +
            'nge of lake volume, volumetric rates for all inflows to and outf' +
            'lows from lakes, total lake conductance, and time-step budget er' +
            'ror. '
          TabOrder = 3
          WordWrap = True
          OnClick = cbGage4Click
        end
      end
    end
    object tabBathymetry: TTabSheet
      Caption = 'Bathymetry'
      ImageIndex = 1
      object rdgLakeTable: TRbwDataGrid4
        Left = 0
        Top = 105
        Width = 455
        Height = 194
        Align = alClient
        ColCount = 3
        FixedCols = 0
        RowCount = 152
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 1
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
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
            CheckMin = True
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
            CheckMin = True
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
          end>
        WordWrapRowCaptions = False
      end
      object pnlBathChoice: TPanel
        Left = 0
        Top = 0
        Width = 455
        Height = 105
        Align = alTop
        TabOrder = 0
        object rgBathChoice: TRadioGroup
          Left = 5
          Top = 3
          Width = 185
          Height = 54
          Caption = 'Bathymetry Choice'
          ItemIndex = 0
          Items.Strings = (
            'Internal'
            'External')
          TabOrder = 0
          OnClick = rgBathChoiceClick
        end
        object feLakeBathymetry: TJvFilenameEdit
          Left = 5
          Top = 63
          Width = 445
          Height = 21
          OnAfterDialog = feLakeBathymetryAfterDialog
          DisabledColor = clBtnFace
          Filter = '(*.lak_bath, *.txt)|*.lak_bath;*.txt|All files (*.*)|*.*'
          Enabled = False
          TabOrder = 1
          Text = ''
          OnKeyUp = feLakeBathymetryKeyUp
        end
      end
    end
    object tabObservations: TTabSheet
      Caption = 'Calibration'
      ImageIndex = 2
      inline framePestObsLak: TframePestObs
        Left = 0
        Top = 0
        Width = 455
        Height = 299
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 455
        ExplicitHeight = 299
        inherited splObservations: TSplitter
          Top = 122
          Width = 455
          ExplicitTop = -17
          ExplicitWidth = 455
        end
        inherited grpDirectObs: TGroupBox
          Width = 455
          Height = 122
          ExplicitWidth = 455
          ExplicitHeight = 122
          inherited frameObservations: TframeGrid
            Width = 451
            Height = 105
            ExplicitWidth = 451
            ExplicitHeight = 105
            inherited Panel: TPanel
              Top = 73
              Width = 451
              ExplicitTop = 73
              ExplicitWidth = 451
              inherited sbAdd: TSpeedButton
                Left = 233
                ExplicitLeft = 233
              end
              inherited sbInsert: TSpeedButton
                Left = 277
                ExplicitLeft = 277
              end
              inherited sbDelete: TSpeedButton
                Left = 320
                ExplicitLeft = 320
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 451
              Height = 73
              ExplicitWidth = 451
              ExplicitHeight = 73
            end
          end
        end
        inherited grpObsComparisons: TGroupBox
          Top = 127
          Width = 455
          ExplicitTop = 127
          ExplicitWidth = 455
          inherited frameObsComparisons: TframeGrid
            Width = 451
            ExplicitWidth = 451
            ExplicitHeight = 155
            inherited Panel: TPanel
              Width = 451
              ExplicitTop = 120
              ExplicitWidth = 451
              inherited sbAdd: TSpeedButton
                Left = 233
                ExplicitLeft = 233
              end
              inherited sbInsert: TSpeedButton
                Left = 277
                ExplicitLeft = 277
              end
              inherited sbDelete: TSpeedButton
                Left = 320
                ExplicitLeft = 320
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 451
              ExplicitWidth = 451
              ExplicitHeight = 120
            end
          end
        end
      end
    end
  end
end
