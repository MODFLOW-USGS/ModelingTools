inherited frmImportBitmap: TfrmImportBitmap
  Left = 315
  Top = 303
  HelpType = htKeyword
  HelpKeyword = 'ImportEdit_Bitmap_Dialog_Box'
  ActiveControl = btnSelectImage
  Caption = 'Import Image'
  ClientHeight = 549
  ClientWidth = 782
  ExplicitWidth = 794
  ExplicitHeight = 587
  TextHeight = 18
  object Splitter1: TSplitter
    Left = 369
    Top = 0
    Height = 421
    ExplicitHeight = 465
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 421
    Width = 782
    Height = 128
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 420
    ExplicitWidth = 778
    DesignSize = (
      782
      128)
    object lblName: TLabel
      Left = 279
      Top = 52
      Width = 42
      Height = 18
      Caption = 'Name'
    end
    object lblNumRows: TLabel
      Left = 87
      Top = 9
      Width = 109
      Height = 18
      Caption = 'Number of rows'
    end
    object btnSelectImage: TButton
      Left = 16
      Top = 48
      Width = 153
      Height = 33
      Caption = 'Select image'
      TabOrder = 3
      OnClick = btnSelectImageClick
    end
    object rgViewDirection: TRadioGroup
      Left = 176
      Top = 40
      Width = 97
      Height = 81
      Caption = 'View from'
      ItemIndex = 0
      Items.Strings = (
        'Top'
        'Front'
        'Side')
      TabOrder = 2
      OnClick = rgViewDirectionClick
    end
    object btnOK: TBitBtn
      Left = 583
      Top = 87
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      Enabled = False
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        04000000000068010000120B0000120B00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      ModalResult = 1
      NumGlyphs = 2
      TabOrder = 7
      OnClick = btnOKClick
      ExplicitLeft = 579
    end
    object btnCancel: TBitBtn
      Left = 680
      Top = 87
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 8
      ExplicitLeft = 676
    end
    object edName: TEdit
      Left = 322
      Top = 49
      Width = 233
      Height = 26
      Cursor = crIBeam
      TabOrder = 4
    end
    object btnHelp: TBitBtn
      Left = 488
      Top = 87
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 6
      OnClick = btnHelpClick
      ExplicitLeft = 484
    end
    object cbVisible: TCheckBox
      Left = 280
      Top = 88
      Width = 101
      Height = 31
      Caption = 'Visible'
      Checked = True
      State = cbChecked
      TabOrder = 9
    end
    object btnImportWorldFile: TButton
      Left = 17
      Top = 87
      Width = 153
      Height = 33
      Caption = 'Import world file'
      TabOrder = 5
      OnClick = btnImportWorldFileClick
    end
    object seNumRows: TJvSpinEdit
      Left = 16
      Top = 6
      Width = 65
      Height = 26
      ButtonKind = bkClassic
      MaxValue = 1000000.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      TabOrder = 0
      OnChange = seNumRowsChange
    end
    object GridPanel1: TGridPanel
      Left = 216
      Top = 6
      Width = 135
      Height = 32
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 33.333333333333340000
        end
        item
          Value = 33.333333333333340000
        end
        item
          Value = 33.333333333333310000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = sbAddRow
          Row = 0
        end
        item
          Column = 1
          Control = sbInsertRow
          Row = 0
        end
        item
          Column = 2
          Control = sbDeleteRow
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      DesignSize = (
        135
        32)
      object sbAddRow: TSpeedButton
        Left = 11
        Top = 5
        Width = 23
        Height = 22
        Hint = 'Add row|Add a row below the bottom row.'
        Anchors = []
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
          CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
          FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
          FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = sbAddRowClick
        ExplicitTop = 6
      end
      object sbInsertRow: TSpeedButton
        Left = 56
        Top = 5
        Width = 23
        Height = 22
        Hint = 'Insert row|Insert a row above the selected row.'
        Anchors = []
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
          FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
          CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
          FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = sbInsertRowClick
        ExplicitTop = 6
      end
      object sbDeleteRow: TSpeedButton
        Left = 101
        Top = 5
        Width = 23
        Height = 22
        Hint = 'Delete row|Delete the selected row.'
        Anchors = []
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
          0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
          0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = sbDeleteRowClick
        ExplicitLeft = 107
        ExplicitTop = 6
      end
    end
  end
  object dgPoints: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 369
    Height = 421
    Align = alLeft
    ColCount = 6
    FixedColor = 14803425
    FixedCols = 1
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
    TabOrder = 0
    OnExit = dgPointsExit
    OnSetEditText = dgPointsSetEditText
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnEndUpdate = dgPointsEndUpdate
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
        AutoAdjustColWidths = True
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
        Format = rcf4Integer
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
        ButtonFont.Name = 'MS Sans Serif'
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
        AutoAdjustColWidths = True
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
        AutoAdjustColWidths = True
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
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -15
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
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end>
    WordWrapRowCaptions = False
    ExplicitHeight = 420
    ColWidths = (
      64
      64
      64
      64
      64
      64)
    RowHeights = (
      24
      24)
  end
  object ScrollBox1: TScrollBox
    Left = 372
    Top = 0
    Width = 410
    Height = 421
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 406
    ExplicitHeight = 420
    object ZoomBox: TQRbwZoomBox2
      Left = 0
      Top = 0
      Width = 575
      Height = 427
      Hint = 'Click on the bitmap and enter real-world coordinates'
      ParentColor = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Exaggeration = 1.000000000000000000
      HorizontalDirection = hdRight
      Image32.Left = 1
      Image32.Top = 1
      Image32.Width = 573
      Image32.Height = 425
      Image32.Align = alClient
      Image32.Bitmap.ResamplerClassName = 'TNearestResampler'
      Image32.BitmapAlign = baTopLeft
      Image32.Color = clWhite
      Image32.ParentColor = False
      Image32.Scale = 1.000000000000000000
      Image32.ScaleMode = smNormal
      Image32.TabOrder = 0
      Image32.OnMouseUp = ZoomBoxImage32MouseUp
      ImmediateResize = True
      Magnification = 1.000000000000000000
      VerticalDirection = vdUp
    end
  end
  object OpenDialogBitmap: TOpenDialog
    Filter = 
      'All Supported Image Formats|*.bmp;*.jpg;*.jpeg;*.png;*.pcx;*.tif' +
      ';*.tiff;*.wmf;*.emf;*.mng;*.pcx;*.tif;*.tiff;|Bitmaps (*.bmp)|*.' +
      'bmp|Jpeg images (*.jpg, *.jpeg)|*.jpg;*.jpeg|Portable Network Gr' +
      'aphics (*.png)|*.png|Paintbrush (*.pcx)|*.pcx|TIFF (*.tif, *.tif' +
      'f)|*.tif;*.tiff|Enhanced Windows Metafiles (*.emf)|*.emf|Windows' +
      ' Metafiles (*.wmf)|*.wmf'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select a bitmap to import'
    Left = 88
    Top = 80
  end
  object odWorldFiled: TOpenDialog
    Filter = 
      'Raster world file (*.tfw;*.tifw;*.jgw;*.jpgw;*.pgw)|*.tfw;*.tifw' +
      ';*.jgw;*.jpgw;*.pgw|CAD world file (*.wld)|*.wld|All files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 136
    Top = 88
  end
end
