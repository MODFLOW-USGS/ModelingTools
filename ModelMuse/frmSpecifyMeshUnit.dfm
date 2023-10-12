inherited frmSpecifyMesh: TfrmSpecifyMesh
  HelpType = htKeyword
  HelpKeyword = 'Specify_Mesh_Dialog_Box'
  Caption = 'Specify Two-Dimensional Mesh'
  ClientHeight = 362
  ClientWidth = 503
  ExplicitWidth = 515
  ExplicitHeight = 400
  TextHeight = 18
  object pgcMeshDesign: TPageControl
    Left = 0
    Top = 0
    Width = 503
    Height = 288
    ActivePage = tabElements
    Align = alClient
    TabOrder = 0
    OnChange = pgcMeshDesignChange
    ExplicitWidth = 499
    ExplicitHeight = 287
    object tabNodes: TTabSheet
      Caption = 'Nodes'
      inline frameNodes: TframeGrid
        Left = 0
        Top = 0
        Width = 499
        Height = 255
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 491
        ExplicitHeight = 254
        inherited Panel: TPanel
          Top = 214
          Width = 499
          ExplicitTop = 213
          ExplicitWidth = 491
          inherited lbNumber: TLabel
            Left = 111
            Width = 119
            Height = 18
            Caption = 'Number of nodes'
            ExplicitLeft = 111
            ExplicitWidth = 119
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 269
            ExplicitLeft = 281
          end
          inherited sbInsert: TSpeedButton
            Left = 306
            ExplicitLeft = 314
          end
          inherited sbDelete: TSpeedButton
            Left = 337
            ExplicitLeft = 349
          end
          inherited seNumber: TJvSpinEdit
            Width = 97
            Height = 26
            OnChange = frameNodesseNumberChange
            ExplicitWidth = 97
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 499
          Height = 214
          ColCount = 3
          FixedCols = 1
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
              AutoAdjustRowHeights = False
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
              Format = rcf4Real
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
          ExplicitWidth = 491
          ExplicitHeight = 213
        end
      end
    end
    object tabElements: TTabSheet
      Caption = 'Elements'
      ImageIndex = 1
      inline frameElements: TframeGrid
        Left = 0
        Top = 0
        Width = 495
        Height = 255
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 491
        ExplicitHeight = 254
        inherited Panel: TPanel
          Top = 214
          Width = 495
          ExplicitTop = 213
          ExplicitWidth = 491
          inherited lbNumber: TLabel
            Left = 111
            Width = 139
            Height = 18
            Caption = 'Number of elements'
            ExplicitLeft = 111
            ExplicitWidth = 139
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 274
            ExplicitLeft = 236
          end
          inherited sbInsert: TSpeedButton
            Left = 306
            ExplicitLeft = 265
          end
          inherited sbDelete: TSpeedButton
            Left = 340
            ExplicitLeft = 294
          end
          inherited seNumber: TJvSpinEdit
            Width = 97
            Height = 26
            OnChange = frameElementsseNumberChange
            ExplicitWidth = 97
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 495
          Height = 214
          ColCount = 5
          FixedCols = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowMoving, goEditing, goAlwaysShowEditor]
          OnBeforeDrawCell = frameElementsGridBeforeDrawCell
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
              AutoAdjustRowHeights = False
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
              Format = rcf4Integer
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
              Format = rcf4Integer
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
              Format = rcf4Integer
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
              Format = rcf4Integer
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end>
          ExplicitWidth = 491
          ExplicitHeight = 213
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 288
    Width = 503
    Height = 74
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 287
    ExplicitWidth = 499
    DesignSize = (
      503
      74)
    object btnCancel: TBitBtn
      Left = 388
      Top = 22
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 384
    end
    object btnOK: TBitBtn
      Left = 291
      Top = 21
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnOKClick
      ExplicitLeft = 287
    end
    object btnHelp: TBitBtn
      Left = 194
      Top = 22
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
      ExplicitLeft = 190
    end
    object rgMeshType: TRadioGroup
      Left = 4
      Top = 6
      Width = 185
      Height = 49
      Caption = 'Mesh type'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        '2D'
        '3D')
      TabOrder = 3
      OnClick = rgMeshTypeClick
    end
  end
end
