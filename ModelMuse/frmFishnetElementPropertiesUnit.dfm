inherited frmFishnetElementProperties: TfrmFishnetElementProperties
  HelpType = htKeyword
  HelpKeyword = 'Fishnet_Quadrilateral_Properti'
  Caption = 'Fishnet Quadrilateral Properties'
  ClientHeight = 442
  ClientWidth = 480
  ExplicitWidth = 492
  ExplicitHeight = 480
  TextHeight = 18
  object btnHelp: TBitBtn
    Left = 84
    Top = 404
    Width = 89
    Height = 33
    HelpType = htKeyword
    Anchors = [akLeft, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnHelpClick
    ExplicitTop = 403
  end
  object btnOK: TBitBtn
    Left = 179
    Top = 404
    Width = 89
    Height = 33
    Anchors = [akLeft, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnOKClick
    ExplicitTop = 403
  end
  object btnCancel: TBitBtn
    Left = 274
    Top = 404
    Width = 91
    Height = 33
    Anchors = [akLeft, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
    ExplicitTop = 403
  end
  object pc1: TPageControl
    Left = 0
    Top = 0
    Width = 480
    Height = 398
    ActivePage = tabSecond
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 476
    ExplicitHeight = 397
    object tabCornerCoordinates: TTabSheet
      Caption = 'Corner Coordinates'
      ImageIndex = 2
      object rdgCornerCoordinates: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 472
        Height = 365
        Align = alClient
        ColCount = 2
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
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
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
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
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
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
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
      end
    end
    object tabFirst: TTabSheet
      Caption = 'First Direction'
      inline frameDiscretization1: TframeDiscretization
        Left = 0
        Top = 0
        Width = 472
        Height = 365
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 472
        ExplicitHeight = 365
        inherited lbl1: TLabel
          Width = 95
          Height = 18
          Caption = 'Discretization'
          ExplicitWidth = 95
          ExplicitHeight = 18
        end
        inherited lbl2: TLabel
          Width = 155
          Height = 18
          Caption = 'Element size multiplier'
          ExplicitWidth = 155
          ExplicitHeight = 18
        end
        inherited pnlDiscritization: TPanel
          Width = 220
          Height = 363
          ExplicitWidth = 220
          ExplicitHeight = 363
          inherited spl1: TSplitter
            Left = 132
            Height = 322
            ExplicitLeft = 136
            ExplicitHeight = 322
          end
          inherited rdgSubLayerBoundaries: TRbwDataGrid4
            Width = 132
            Height = 322
            ExplicitWidth = 132
            ExplicitHeight = 322
          end
          inherited pnl1: TPanel
            Width = 220
            ExplicitWidth = 224
            inherited lbl3: TLabel
              Width = 142
              Height = 36
              Caption = 'Discretization within quadrilateral'
              ExplicitWidth = 142
              ExplicitHeight = 36
            end
          end
          inherited pnlPaintboxParent: TPanel
            Left = 135
            Height = 322
            ExplicitLeft = 139
            ExplicitHeight = 322
            inherited pbSubLayers: TPaintBox
              Height = 287
              ExplicitHeight = 287
            end
            inherited sbInsertLine: TSpeedButton
              Top = 293
              ExplicitTop = 293
            end
            inherited sbMoveLine: TSpeedButton
              Top = 293
              ExplicitTop = 293
            end
            inherited sbDeleteLine: TSpeedButton
              Top = 293
              ExplicitTop = 293
            end
          end
        end
        inherited rgMethod: TRadioGroup
          Height = 247
          Items.Strings = (
            'Uniform spacing'
            'Increase forward'
            'Increase backward'
            'Increase toward middle'
            'Increase toward edges'
            'Custom')
          ExplicitHeight = 247
        end
      end
    end
    object tabSecond: TTabSheet
      Caption = 'Second Direction'
      ImageIndex = 1
      inline frameDiscretization2: TframeDiscretization
        Left = 0
        Top = 0
        Width = 472
        Height = 365
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 468
        ExplicitHeight = 364
        inherited lbl1: TLabel
          Width = 95
          Height = 18
          Caption = 'Discretization'
          ExplicitWidth = 95
          ExplicitHeight = 18
        end
        inherited lbl2: TLabel
          Width = 155
          Height = 18
          Caption = 'Element size multiplier'
          ExplicitWidth = 155
          ExplicitHeight = 18
        end
        inherited pnlDiscritization: TPanel
          Width = 220
          Height = 363
          ExplicitWidth = 216
          ExplicitHeight = 362
          inherited spl1: TSplitter
            Left = 132
            Height = 322
            ExplicitLeft = 136
            ExplicitHeight = 322
          end
          inherited rdgSubLayerBoundaries: TRbwDataGrid4
            Width = 132
            Height = 322
            ExplicitWidth = 128
            ExplicitHeight = 321
          end
          inherited pnl1: TPanel
            Width = 220
            ExplicitWidth = 216
            inherited lbl3: TLabel
              Width = 220
              Height = 41
              Caption = 'Discretization within quadrilateral'
              ExplicitWidth = 142
              ExplicitHeight = 36
            end
          end
          inherited pnlPaintboxParent: TPanel
            Left = 135
            Height = 322
            ExplicitLeft = 131
            ExplicitHeight = 321
            inherited pbSubLayers: TPaintBox
              Height = 287
              ExplicitHeight = 287
            end
            inherited sbInsertLine: TSpeedButton
              Top = 293
              ExplicitTop = 293
            end
            inherited sbMoveLine: TSpeedButton
              Top = 293
              ExplicitTop = 293
            end
            inherited sbDeleteLine: TSpeedButton
              Top = 293
              ExplicitTop = 293
            end
          end
        end
        inherited rgMethod: TRadioGroup
          Height = 247
          Items.Strings = (
            'Uniform spacing'
            'Increase forward'
            'Increase backward'
            'Increase toward middle'
            'Increase toward edges'
            'Custom')
          ExplicitHeight = 246
        end
      end
    end
  end
end
