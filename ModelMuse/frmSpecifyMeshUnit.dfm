inherited frmSpecifyMesh: TfrmSpecifyMesh
  HelpType = htKeyword
  HelpKeyword = 'Specify_Mesh_Dialog_Box'
  Caption = 'Specify Two-Dimensional Mesh'
  ClientHeight = 362
  ClientWidth = 503
  ExplicitWidth = 521
  ExplicitHeight = 407
  PixelsPerInch = 96
  TextHeight = 18
  object pgcMeshDesign: TPageControl
    Left = 0
    Top = 0
    Width = 503
    Height = 312
    ActivePage = tabNodes
    Align = alClient
    TabOrder = 0
    OnChange = pgcMeshDesignChange
    object tabNodes: TTabSheet
      Caption = 'Nodes'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      inline frameNodes: TframeGrid
        Left = 0
        Top = 0
        Width = 495
        Height = 279
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 495
        ExplicitHeight = 279
        inherited Panel: TPanel
          Top = 238
          Width = 495
          ExplicitTop = 238
          ExplicitWidth = 495
          inherited lbNumber: TLabel
            Width = 119
            Height = 18
            Caption = 'Number of nodes'
            ExplicitWidth = 119
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 236
            ExplicitLeft = 236
          end
          inherited sbInsert: TSpeedButton
            Left = 265
            ExplicitLeft = 265
          end
          inherited sbDelete: TSpeedButton
            Left = 294
            ExplicitLeft = 294
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            OnChange = frameNodesseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 495
          Height = 238
          ColCount = 3
          FixedCols = 1
          Columns = <
            item
              AutoAdjustRowHeights = True
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
          ExplicitWidth = 495
          ExplicitHeight = 238
        end
      end
    end
    object tabElements: TTabSheet
      Caption = 'Elements'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      inline frameElements: TframeGrid
        Left = 0
        Top = 0
        Width = 495
        Height = 279
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 495
        ExplicitHeight = 279
        inherited Panel: TPanel
          Top = 238
          Width = 495
          ExplicitTop = 238
          ExplicitWidth = 495
          inherited lbNumber: TLabel
            Width = 139
            Height = 18
            Caption = 'Number of elements'
            ExplicitWidth = 139
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 283
            ExplicitLeft = 236
          end
          inherited sbInsert: TSpeedButton
            Left = 317
            ExplicitLeft = 265
          end
          inherited sbDelete: TSpeedButton
            Left = 352
            ExplicitLeft = 294
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            OnChange = frameElementsseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 495
          Height = 238
          ColCount = 5
          FixedCols = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowMoving, goEditing, goAlwaysShowEditor]
          OnBeforeDrawCell = frameElementsGridBeforeDrawCell
          Columns = <
            item
              AutoAdjustRowHeights = True
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
          ExplicitWidth = 495
          ExplicitHeight = 238
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 312
    Width = 503
    Height = 50
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      503
      50)
    object btnCancel: TBitBtn
      Left = 393
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 296
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 199
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
    end
  end
end
