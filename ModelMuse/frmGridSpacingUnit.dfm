inherited frmGridSpacing: TfrmGridSpacing
  Left = 674
  Top = 421
  HelpType = htKeyword
  HelpKeyword = 'Grid_Spacing_Dialog_Box'
  ActiveControl = pcSubdivide
  Caption = 'Grid Spacing'
  ClientHeight = 453
  ClientWidth = 461
  KeyPreview = True
  Position = poOwnerFormCenter
  OnKeyPress = FormKeyPress
  ExplicitWidth = 477
  ExplicitHeight = 492
  PixelsPerInch = 96
  TextHeight = 18
  object pcSubdivide: TPageControl
    Left = 0
    Top = 129
    Width = 461
    Height = 283
    ActivePage = tabLayers
    Align = alClient
    TabOrder = 1
    object tabColumns: TTabSheet
      Caption = 'Columns (X'#39')'
      object pnlColumns: TPanel
        Left = 0
        Top = 169
        Width = 453
        Height = 81
        Align = alBottom
        ParentColor = True
        TabOrder = 1
        DesignSize = (
          453
          81)
        object lblColNumNodes: TLabel
          Left = 8
          Top = 8
          Width = 310
          Height = 18
          Caption = 'Number of nodes in the X (column) direction: '
        end
        object lblColDefaultSpacing: TLabel
          Left = 8
          Top = 44
          Width = 108
          Height = 18
          Caption = 'Default spacing'
        end
        object rdeSpacingColumns: TRbwDataEntry
          Left = 346
          Top = 40
          Width = 102
          Height = 28
          Cursor = crIBeam
          Anchors = [akTop, akRight]
          TabOrder = 1
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object seColumns: TJvSpinEdit
          Left = 346
          Top = 9
          Width = 102
          Height = 26
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          Anchors = [akTop, akRight]
          TabOrder = 0
          OnChange = seColumnsChange
        end
      end
      object dgColumns: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 453
        Height = 169
        Align = alClient
        ColCount = 2
        DefaultColWidth = 200
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 0
        OnKeyUp = dgKeyUp
        OnMouseDown = dgMouseDown
        OnMouseMove = dgMouseMove
        OnMouseUp = dgMouseUp
        OnSelectCell = dgSelectCell
        OnSetEditText = dgSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = True
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
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
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
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
        RowHeights = (
          24
          24
          24
          24
          24)
      end
    end
    object tabRows: TTabSheet
      Caption = 'Rows (Y'#39')'
      ImageIndex = 1
      object pnlRows: TPanel
        Left = 0
        Top = 169
        Width = 453
        Height = 81
        Align = alBottom
        ParentColor = True
        TabOrder = 1
        DesignSize = (
          453
          81)
        object lblRowNumNodes: TLabel
          Left = 8
          Top = 8
          Width = 284
          Height = 18
          Caption = 'Number of nodes in the Y (row) direction: '
        end
        object lblRowDefaultSpacing: TLabel
          Left = 8
          Top = 44
          Width = 108
          Height = 18
          Caption = 'Default spacing'
        end
        object rdeSpacingRows: TRbwDataEntry
          Left = 346
          Top = 40
          Width = 102
          Height = 28
          Cursor = crIBeam
          Anchors = [akTop, akRight]
          TabOrder = 1
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object seRows: TJvSpinEdit
          Left = 346
          Top = 9
          Width = 102
          Height = 26
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          Anchors = [akTop, akRight]
          TabOrder = 0
          OnChange = seRowsChange
        end
      end
      object dgRows: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 453
        Height = 169
        Align = alClient
        ColCount = 2
        DefaultColWidth = 200
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 0
        OnKeyUp = dgKeyUp
        OnMouseDown = dgMouseDown
        OnMouseMove = dgMouseMove
        OnMouseUp = dgMouseUp
        OnSelectCell = dgSelectCell
        OnSetEditText = dgSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
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
            ButtonFont.Name = 'MS Sans Serif'
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
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
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
        RowHeights = (
          24
          24
          24
          24
          24)
      end
    end
    object tabLayers: TTabSheet
      Caption = 'Layers (Z)'
      ImageIndex = 2
      object pnlLayers: TPanel
        Left = 0
        Top = 169
        Width = 453
        Height = 81
        Align = alBottom
        ParentColor = True
        TabOrder = 1
        DesignSize = (
          453
          81)
        object lblLayNumNodes: TLabel
          Left = 8
          Top = 8
          Width = 292
          Height = 18
          Caption = 'Number of nodes in the Z (layer) direction: '
        end
        object lblLayDefaultSpacing: TLabel
          Left = 8
          Top = 44
          Width = 108
          Height = 18
          Caption = 'Default spacing'
        end
        object rdeSpacingLayers: TRbwDataEntry
          Left = 346
          Top = 40
          Width = 102
          Height = 28
          Cursor = crIBeam
          Anchors = [akTop, akRight]
          TabOrder = 1
          Text = '10'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object seLayers: TJvSpinEdit
          Left = 346
          Top = 9
          Width = 102
          Height = 26
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          Anchors = [akTop, akRight]
          TabOrder = 0
          OnChange = seLayersChange
        end
      end
      object dgLayers: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 453
        Height = 169
        Align = alClient
        ColCount = 2
        DefaultColWidth = 200
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 0
        OnKeyUp = dgKeyUp
        OnMouseDown = dgMouseDown
        OnMouseMove = dgMouseMove
        OnMouseUp = dgMouseUp
        OnSelectCell = dgSelectCell
        OnSetEditText = dgSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
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
            ButtonFont.Name = 'MS Sans Serif'
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
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
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
        RowHeights = (
          24
          24
          24
          24
          24)
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 412
    Width = 461
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      461
      41)
    object btnCancel: TBitBtn
      Left = 361
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 264
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 168
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
  object pnlDescribe: TPanel
    Left = 0
    Top = 0
    Width = 461
    Height = 129
    Align = alTop
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      461
      129)
    object lblDescribe: TLabel
      Left = 8
      Top = 8
      Width = 444
      Height = 54
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Type in new positions of element boundaries, or drag them to the' +
        ' end and change the number of items to delete them.  Use Ctrl-V ' +
        'to paste.'
      WordWrap = True
      ExplicitWidth = 422
    end
    object lblModel: TLabel
      Left = 409
      Top = 100
      Width = 43
      Height = 18
      Anchors = [akTop, akRight]
      Caption = 'Model'
      ExplicitLeft = 387
    end
    object comboModel: TComboBox
      Left = 8
      Top = 97
      Width = 395
      Height = 26
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = comboModelChange
    end
  end
end
