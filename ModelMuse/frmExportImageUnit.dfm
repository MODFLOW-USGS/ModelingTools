inherited frmExportImage: TfrmExportImage
  HelpType = htKeyword
  HelpKeyword = 'Export_Image_Dialog_Box'
  Caption = 'Export Image'
  ClientHeight = 555
  ClientWidth = 721
  ShowHint = True
  OnClose = FormClose
  ExplicitTop = -120
  ExplicitWidth = 737
  ExplicitHeight = 594
  PixelsPerInch = 96
  TextHeight = 18
  object JvNetscapeSplitter2: TJvNetscapeSplitter
    Left = 249
    Top = 0
    Height = 450
    Align = alLeft
    MinSize = 3
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 300
    ExplicitTop = -6
    ExplicitHeight = 448
  end
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 450
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object CatPanelGroup: TCategoryPanelGroup
      Left = 0
      Top = 0
      Width = 249
      Height = 450
      VertScrollBar.Tracking = True
      Align = alClient
      HeaderFont.Charset = DEFAULT_CHARSET
      HeaderFont.Color = clWindowText
      HeaderFont.Height = -11
      HeaderFont.Name = 'Tahoma'
      HeaderFont.Style = []
      TabOrder = 0
      object cpAnimation: TCategoryPanel
        Top = 416
        Height = 30
        Caption = '&Animation'
        Collapsed = True
        TabOrder = 0
        OnExpand = cpAnimationExpand
        ExplicitTop = 60
        ExpandedHeight = 386
        object pnlAnimation: TPanel
          Left = 0
          Top = 0
          Width = 243
          Height = 0
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object JvNetscapeSplitter1: TJvNetscapeSplitter
            Left = 0
            Top = 168
            Width = 243
            Height = 10
            Cursor = crVSplit
            Align = alTop
            MinSize = 3
            Maximized = False
            Minimized = False
            ButtonCursor = crDefault
            ExplicitLeft = 1
            ExplicitTop = 1
            ExplicitWidth = 174
          end
          object Panel1: TPanel
            Left = 0
            Top = -84
            Width = 243
            Height = 84
            Align = alBottom
            TabOrder = 0
            DesignSize = (
              243
              84)
            object rgDisplayChoice: TRadioGroup
              Left = 3
              Top = 6
              Width = 236
              Height = 43
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Display choice'
              Columns = 2
              ItemIndex = 0
              Items.Strings = (
                'Color grid'
                'Contour data')
              TabOrder = 0
            end
            object btnPreview: TButton
              Left = 3
              Top = 55
              Width = 75
              Height = 25
              Caption = 'Preview'
              TabOrder = 1
              OnClick = btnPreviewClick
            end
            object btnStop: TButton
              Left = 166
              Top = 55
              Width = 75
              Height = 25
              Caption = 'Stop'
              TabOrder = 3
              OnClick = btnStopClick
            end
            object btnSaveMultipleImages: TBitBtn
              Left = 84
              Top = 55
              Width = 75
              Height = 25
              Caption = 'Save'
              Glyph.Data = {
                76010000424D7601000000000000760000002800000020000000100000000100
                04000000000000010000120B0000120B00001000000000000000000000000000
                800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
                0000377777777777777703030303030303037F7F7F7F7F7F7F7F000000000000
                00007777777777777777933393303933337073F37F37F73F3377393393303393
                379037FF7F37F37FF777379793303379793037777337F3777737339933303339
                93303377F3F7F3F77F3733993930393993303377F737F7377FF7399993303399
                999037777337F377777793993330333393307377FF37F3337FF7333993303333
                993033377F37F33377F7333993303333993033377337F3337737333333303333
                33303FFFFFF7FFFFFFF700000000000000007777777777777777030303030303
                03037F7F7F7F7F7F7F7F00000000000000007777777777777777}
              NumGlyphs = 2
              TabOrder = 2
              OnClick = btnSaveMultipleImagesClick
            end
          end
          object rdgDataSets: TRbwDataGrid4
            Left = 0
            Top = 178
            Width = 243
            Height = 98
            Align = alClient
            ColCount = 2
            FixedCols = 1
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goAlwaysShowEditor]
            TabOrder = 2
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            ColorRangeSelection = False
            Columns = <
              item
                AutoAdjustRowHeights = False
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
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = False
            ColWidths = (
              19
              64)
          end
          object vstDataSets: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 243
            Height = 168
            Align = alTop
            Colors.BorderColor = 15987699
            Colors.DisabledColor = clGray
            Colors.DropMarkColor = 15385233
            Colors.DropTargetColor = 15385233
            Colors.DropTargetBorderColor = 15385233
            Colors.FocusedSelectionColor = 15385233
            Colors.FocusedSelectionBorderColor = 15385233
            Colors.GridLineColor = 15987699
            Colors.HeaderHotColor = clBlack
            Colors.HotColor = clBlack
            Colors.SelectionRectangleBlendColor = 15385233
            Colors.SelectionRectangleBorderColor = 15385233
            Colors.SelectionTextColor = clBlack
            Colors.TreeLineColor = 9471874
            Colors.UnfocusedColor = clGray
            Colors.UnfocusedSelectionColor = 13421772
            Colors.UnfocusedSelectionBorderColor = 13421772
            Header.AutoSizeIndex = 0
            Header.MainColumn = -1
            PopupMenu = pmChangeStates
            TabOrder = 1
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
            TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
            OnChecked = vstDataSetsChecked
            OnGetText = vstDataSetsGetText
            OnGetNodeDataSize = vstDataSetsGetNodeDataSize
            OnInitNode = vstDataSetsInitNode
            Columns = <>
          end
        end
      end
      object cpText: TCategoryPanel
        Top = 386
        Height = 30
        Caption = '&Text'
        Collapsed = True
        TabOrder = 1
        OnExpand = cpTextExpand
        ExplicitTop = 30
        ExpandedHeight = 386
        object pnlText: TPanel
          Left = 0
          Top = 0
          Width = 243
          Height = 0
          Align = alClient
          TabOrder = 0
          ExplicitLeft = -3
          ExplicitTop = -3
          ExplicitHeight = 360
          object lblTitle: TLabel
            Left = 11
            Top = 2
            Width = 28
            Height = 18
            Caption = 'T&itle'
            FocusControl = memoTitle
          end
          object sbText: TSpeedButton
            Left = 11
            Top = 182
            Width = 23
            Height = 22
            Hint = 'Edit text'
            GroupIndex = 1
            Down = True
            Glyph.Data = {
              4E010000424D4E01000000000000760000002800000012000000120000000100
              040000000000D800000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
              FFFFFF000000FFFFFF000000FFFFFF000000FFFFFFF8008FFFFFFF000000FFFF
              FFFF00FFFFFFFF000000FFFFFFFF00FFFFFFFF000000FFFFFFFF00FFFFFFFF00
              0000FFFFFFFF00FFFFFFFF000000FFFFFFFF00FFFFFFFF000000FFFFFFFF00FF
              FFFFFF000000FFFFFFFF00FFFFFFFF000000FFFFFFFF00FFFFFFFF000000FFFF
              FFFF00FFFFFFFF000000FFFFFFFF00FFFFFFFF000000FF7FFFFF00FFFFF7FF00
              0000FF08FFFF00FFFF80FF000000FF07FFFF00FFFF70FF000000FF0000000000
              0000FF000000FFFFFFFFFFFFFFFFFF000000}
          end
          object sbSelect: TSpeedButton
            Left = 40
            Top = 182
            Width = 23
            Height = 22
            Hint = 'Select text'
            GroupIndex = 1
            Glyph.Data = {
              76010000424D7601000000000000760000002800000020000000100000000100
              04000000000000010000120B0000120B00001000000000000000000000000000
              800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
              333333333333333FFF3333333333333707333333333333F777F3333333333370
              9033333333F33F7737F33333373337090733333337F3F7737733333330037090
              73333333377F7737733333333090090733333333373773773333333309999073
              333333337F333773333333330999903333333333733337F33333333099999903
              33333337F3333F7FF33333309999900733333337333FF7773333330999900333
              3333337F3FF7733333333309900333333333337FF77333333333309003333333
              333337F773333333333330033333333333333773333333333333333333333333
              3333333333333333333333333333333333333333333333333333}
            NumGlyphs = 2
          end
          object lblColorLegendTitle: TLabel
            Left = 11
            Top = 247
            Width = 117
            Height = 18
            Caption = '&Color legend title'
            FocusControl = edColorLegendTitle
          end
          object lblContourLegendTitle: TLabel
            Left = 11
            Top = 304
            Width = 134
            Height = 18
            Caption = 'C&ontour legend title'
            FocusControl = edContourLegendTitle
          end
          object memoTitle: TMemo
            Left = 11
            Top = 26
            Width = 222
            Height = 89
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
            OnChange = memoTitleChange
          end
          object btnTitleFont: TButton
            Left = 11
            Top = 122
            Width = 75
            Height = 25
            Caption = 'Title font'
            TabOrder = 1
            OnClick = btnTitleFontClick
          end
          object btnFont: TButton
            Left = 85
            Top = 182
            Width = 75
            Height = 25
            Caption = 'Text font'
            TabOrder = 2
            OnClick = btnFontClick
          end
          object btnScaleFont: TButton
            Left = 16
            Top = 216
            Width = 90
            Height = 25
            Caption = 'Scale font'
            TabOrder = 3
            OnClick = btnScaleFontClick
          end
          object edColorLegendTitle: TEdit
            Left = 19
            Top = 272
            Width = 222
            Height = 26
            TabOrder = 4
            Text = 'Color legend'
            OnChange = edColorLegendTitleChange
          end
          object edContourLegendTitle: TEdit
            Left = 19
            Top = 328
            Width = 222
            Height = 26
            TabOrder = 5
            Text = 'Contour legend'
            OnChange = edContourLegendTitleChange
          end
        end
      end
      object cpView: TCategoryPanel
        Top = 0
        Height = 386
        Caption = '&View'
        TabOrder = 2
        OnExpand = cpViewExpand
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 243
          Height = 360
          Align = alClient
          TabOrder = 0
          object lblSelectedView: TLabel
            Left = 4
            Top = 0
            Width = 97
            Height = 18
            Caption = 'Selected view'
          end
          object lblImageHeight: TLabel
            Left = 4
            Top = 64
            Width = 191
            Height = 18
            Caption = 'Model image &height (pixels)'
          end
          object lblImageWidth: TLabel
            Left = 4
            Top = 128
            Width = 185
            Height = 18
            Caption = 'Model image &width (pixels)'
          end
          object comboView: TComboBox
            Left = 4
            Top = 21
            Width = 195
            Height = 26
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 0
            Text = 'Top'
            OnChange = comboViewChange
            Items.Strings = (
              'Top'
              'Front'
              'Side')
          end
          object seImageHeight: TJvSpinEdit
            Left = 4
            Top = 85
            Width = 121
            Height = 26
            CheckMinValue = True
            ButtonKind = bkClassic
            Increment = 100.000000000000000000
            TabOrder = 1
            OnChange = seImageHeightChange
          end
          object seImageWidth: TJvSpinEdit
            Left = 4
            Top = 149
            Width = 121
            Height = 26
            CheckMinValue = True
            ButtonKind = bkClassic
            Increment = 100.000000000000000000
            TabOrder = 2
            OnChange = seImageWidthChange
          end
          object cbShowColoredGridLines: TCheckBox
            Left = 4
            Top = 191
            Width = 195
            Height = 17
            Caption = 'Show colored grid lines'
            TabOrder = 3
            OnClick = cbShowColoredGridLinesClick
          end
          object cbColorLegend: TCheckBox
            Left = 5
            Top = 214
            Width = 190
            Height = 17
            Caption = 'Show color legend'
            Checked = True
            State = cbChecked
            TabOrder = 4
            OnClick = cbColorLegendClick
          end
          object cbContourLegend: TCheckBox
            Left = 4
            Top = 237
            Width = 190
            Height = 17
            Caption = 'Show contour legend'
            Checked = True
            State = cbChecked
            TabOrder = 5
            OnClick = cbContourLegendClick
          end
          object cbHorizontalScale: TCheckBox
            Left = 4
            Top = 304
            Width = 190
            Height = 17
            Caption = 'Horizontal scale'
            Checked = True
            State = cbChecked
            TabOrder = 8
            OnClick = cbHorizontalScaleClick
          end
          object cbVerticalScale: TCheckBox
            Left = 4
            Top = 327
            Width = 190
            Height = 17
            Caption = 'Vertical scale'
            Checked = True
            State = cbChecked
            TabOrder = 9
            OnClick = cbVerticalScaleClick
          end
          object cbHeadObsLegend: TCheckBox
            Left = 4
            Top = 260
            Width = 238
            Height = 17
            Caption = 'Show head observation legend'
            Checked = True
            State = cbChecked
            TabOrder = 6
            OnClick = cbHeadObsLegendClick
          end
          object cbEndpoints: TCheckBox
            Left = 4
            Top = 281
            Width = 217
            Height = 17
            Caption = 'Show endpoints legend'
            Checked = True
            State = cbChecked
            TabOrder = 7
            OnClick = cbEndpointsClick
          end
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 450
    Width = 721
    Height = 105
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      721
      105)
    object lblSavedSettings: TLabel
      Left = 6
      Top = 8
      Width = 103
      Height = 18
      Caption = '&Saved settings'
      FocusControl = comboSavedSettings
    end
    object btnHelp: TBitBtn
      Left = 629
      Top = 6
      Width = 87
      Height = 26
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnHelpClick
    end
    object btnClose: TBitBtn
      Left = 629
      Top = 38
      Width = 87
      Height = 26
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 7
      OnClick = btnCloseClick
    end
    object btnSaveSettings: TButton
      Left = 223
      Top = 6
      Width = 87
      Height = 58
      Caption = 'S&ave settings'
      TabOrder = 0
      WordWrap = True
      OnClick = btnSaveSettingsClick
    end
    object comboSavedSettings: TComboBox
      Left = 7
      Top = 32
      Width = 195
      Height = 26
      AutoComplete = False
      TabOrder = 5
      Text = '(none)'
      OnChange = comboSavedSettingsChange
      OnCloseUp = comboSavedSettingsCloseUp
      OnDropDown = comboSavedSettingsDropDown
    end
    object btnRefresh: TBitBtn
      Left = 408
      Top = 6
      Width = 87
      Height = 58
      Anchors = [akTop, akRight]
      Caption = '&Refresh'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
        3333333777333777FF33339993707399933333773337F3777FF3399933000339
        9933377333777F3377F3399333707333993337733337333337FF993333333333
        399377F33333F333377F993333303333399377F33337FF333373993333707333
        333377F333777F333333993333101333333377F333777F3FFFFF993333000399
        999377FF33777F77777F3993330003399993373FF3777F37777F399933000333
        99933773FF777F3F777F339993707399999333773F373F77777F333999999999
        3393333777333777337333333999993333333333377777333333}
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnRefreshClick
    end
    object btnManageSettings: TButton
      Left = 316
      Top = 6
      Width = 87
      Height = 58
      Caption = '&Manage settings'
      TabOrder = 1
      WordWrap = True
      OnClick = btnManageSettingsClick
    end
    object btnSaveImage1: TJvBitBtn
      Left = 501
      Top = 38
      Width = 122
      Height = 26
      Anchors = [akTop, akRight]
      Caption = 'Save &image'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
        7700333333337777777733333333008088003333333377F73377333333330088
        88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
        000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
        FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
        99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
        99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
        99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
        93337FFFF7737777733300000033333333337777773333333333}
      NumGlyphs = 2
      TabOrder = 6
      OnClick = btnSaveImageClick
      HotTrackFont.Charset = ANSI_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -16
      HotTrackFont.Name = 'Arial'
      HotTrackFont.Pitch = fpVariable
      HotTrackFont.Style = []
    end
    object JvBitBtn1: TJvBitBtn
      Left = 501
      Top = 6
      Width = 122
      Height = 26
      Anchors = [akTop, akRight]
      Caption = 'Copy image'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003333330B7FFF
        FFB0333333777F3333773333330B7FFFFFB0333333777F3333773333330B7FFF
        FFB0333333777F3333773333330B7FFFFFB03FFFFF777FFFFF77000000000077
        007077777777777777770FFFFFFFF00077B07F33333337FFFF770FFFFFFFF000
        7BB07F3FF3FFF77FF7770F00F000F00090077F77377737777F770FFFFFFFF039
        99337F3FFFF3F7F777FF0F0000F0F09999937F7777373777777F0FFFFFFFF999
        99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
        99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
        93337FFFF7737777733300000033333333337777773333333333}
      NumGlyphs = 2
      TabOrder = 3
      OnClick = JvBitBtn1Click
      HotTrackFont.Charset = ANSI_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -16
      HotTrackFont.Name = 'Arial'
      HotTrackFont.Pitch = fpVariable
      HotTrackFont.Style = []
    end
    object btnCopySettings: TBitBtn
      Left = 224
      Top = 64
      Width = 146
      Height = 33
      Caption = 'Copy Settings'
      Glyph.Data = {
        F6060000424DF606000000000000360000002800000018000000180000000100
        180000000000C0060000120B0000120B00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFF4F4F4C3CCDF
        F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F6F6F6BFBF
        BFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBF
        BFBF6C6C6CC3CCDFE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BA
        E9D0BAE9D0BABFBFBFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFBFBFBFF5F5F5C3CCDFF5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5
        F5F5F4F4F4F4F4F4F3F3F3F5F5F5BFBFBFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFE9D0BAC3CCDFE9D0BAE9D0BAE9D0
        BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BABFBFBFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFF7F7F7C3CCDF
        F7F7F7F7F7F7F8F8F8F7F7F7F7F7F7F7F7F7F7F7F7F6F6F6F6F6F6F7F7F7BFBF
        BFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFBFBFBFBFBFBFBFBFBFACACACBF
        BFBFE9D0BAC3CCDFE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BA
        E9D0BAE9D0BABFBFBFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFF4F4F4C3CC
        DFF4F4F4DBDBDBBFBFBFF9F9F9C3CCDFF9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9
        F9F9F9F9F9F8F8F8F8F8F8F8F8F8BFBFBFFF00FFFF00FFFF00FFFF00FFFF00FF
        BFBFBF6C6C6CC3CCDFE9D0BAD1BBA7BFBFBFE9D0BAC3CCDFE9D0BAE9D0BAE9D0
        BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BABFBFBFFF00FFFF00FFFF
        00FFFF00FFFF00FFBFBFBFF5F5F5C3CCDFF5F5F5DCDCDCBFBFBF6C6C6CC3CCDF
        FBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFAFAFAFAFAFAF9F9F9F9F9F9FAFAFABFBF
        BFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFE9D0BAC3CCDFE9D0BAD1BBA7BF
        BFBFE9D0BAC3CCDFE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BA
        E9D0BAE9D0BABFBFBFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFF7F7F7C3CC
        DFF7F7F7DEDEDEBFBFBFFCFCFCC3CCDFFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFBFBFBFBFBFBFAFAFAFBFBFBBFBFBFFF00FFFF00FFFF00FFFF00FFFF00FF
        BFBFBFE9D0BAC3CCDFE9D0BAD1BBA7BFBFBFE9D0BAC3CCDFE9D0BAE9D0BAE9D0
        BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BABFBFBFFF00FFFF00FFFF
        00FFFF00FFFF00FFBFBFBFF9F9F9C3CCDFF9F9F9E0E0E0BFBFBFFDFDFDC3CCDF
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFCFCFCEAEAEAE4E4E4ECECECBFBF
        BFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFE9D0BAC3CCDFE9D0BAD1BBA7BF
        BFBFE9D0BAC3CCDFE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BABFBFBF
        BFBFBFBFBFBFBFBFBFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBF6C6C6CC3CC
        DFFBFBFBE1E1E1BFBFBF6C6C6CC3CCDFFDFDFDFEFEFEFEFEFEFEFEFEFDFDFDFD
        FDFDFBFBFBBFBFBFF1D5F1BFBFBFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        BFBFBFE9D0BAC3CCDFE9D0BAD1BBA7BFBFBFFFFFFFC3CCDFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFCFCFCBFBFBFBFBFBFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFBFBFBFFCFCFCC3CCDFFCFCFCE2E2E2BFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFE9D0BAC3CCDFE9D0BAD1BBA7D1
        BBA7D1BBA7D1BBA7D1BBA7D1BBA7D1BBA7D1BBA7D1BBA7ACACACFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFFDFDFDC3CC
        DFFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFCFCFCEAEAEAE4E4E4ECECECBF
        BFBFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        BFBFBFE9D0BAC3CCDFE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BABFBF
        BFBFBFBFBFBFBFBFBFBFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFC0C0C0727272C3CCDFFDFDFDFEFEFEFEFEFEFEFEFEFDFDFD
        FDFDFDFBFBFBBFBFBFF1D5F1BFBFBFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFFFFFFFC3CCDFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFCFCFCBFBFBFBFBFBFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      TabOrder = 8
      OnClick = btnCopySettingsClick
    end
    object btnPasteSettings: TBitBtn
      Left = 376
      Top = 64
      Width = 146
      Height = 33
      Caption = 'Paste Settings'
      Glyph.Data = {
        F6060000424DF606000000000000360000002800000018000000180000000100
        180000000000C0060000120B0000120B00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FF489FBF479EBF489EBF489DBE479DBE397E98397E98397E98
        397E98397E98397E98397E98397E98397E98397E98397E98397E98FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FF49A0C171B5D26DB6D26EB8D36EB8D35A
        93ABBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFFF00FFFF00FFFF00FFFF00FF49A0C16CB4D26DB6
        D26EB8D36EB8D35A93ABBFBFBFF4F4F4C3CCDFF4F4F4F4F4F4F4F4F4F4F4F4F4
        F4F4F4F4F4F4F4F4F4F4F4F4F4F4F6F6F6BFBFBFFF00FFFF00FFFF00FFFF00FF
        49A2C26CB5D36DB7D46EBAD570BAD75A96ACBFBFBF6C6C6CC3CCDFE9D0BAE9D0
        BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BABFBFBFFF00FFFF
        00FFFF00FFFF00FF4AA3C46EB9D46FB9D670BAD772BCD75C99ADBFBFBFF5F5F5
        C3CCDFF5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F4F4F4F4F4F4F3F3F3F5F5
        F5BFBFBFFF00FFFF00FFFF00FFFF00FF4CA6C670BAD770BBD772BDD873BED95C
        9AAEBFBFBFE9D0BAC3CCDFE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BA
        E9D0BAE9D0BAE9D0BABFBFBFFF00FFFF00FFFF00FFFF00FF4EA8C871BCD872BD
        D973BFD973C1DA5E9BB1BFBFBFF7F7F7C3CCDFF7F7F7F7F7F7F8F8F8F7F7F7F7
        F7F7F7F7F7F7F7F7F6F6F6F6F6F6F7F7F7BFBFBFFF00FFFF00FFFF00FFFF00FF
        4FABCA74BED873C1DA76C2DB75C1DD5F9DB0BFBFBFE9D0BAC3CCDFE9D0BAE9D0
        BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BABFBFBFFF00FFFF
        00FFFF00FFFF00FF50ADCC74C2DB75C2DC75C4DC77C4DD609EB2BFBFBFF9F9F9
        C3CCDFF9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F8F8F8F8F8F8F8F8
        F8BFBFBFFF00FFFF00FFFF00FFFF00FF52B0CF76C3DC76C5DD77C5DF79C6DE62
        A0B3BFBFBFE9D0BAC3CCDFE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BA
        E9D0BAE9D0BAE9D0BABFBFBFFF00FFFF00FFFF00FFFF00FF53B2D176C5DE78C5
        DF79C7E07BCAE263A2B5BFBFBF6C6C6CC3CCDFFBFBFBFBFBFBFBFBFBFBFBFBFB
        FBFBFAFAFAFAFAFAF9F9F9F9F9F9FAFAFABFBFBFFF00FFFF00FFFF00FFFF00FF
        55B5D37AC7DF79C7E17BCAE27BCAE264A5B6BFBFBFE9D0BAC3CCDFE9D0BAE9D0
        BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BABFBFBFFF00FFFF
        00FFFF00FFFF00FF57B7D579C8E27BC9E27CCCE37DCEE465A5B7BFBFBFFCFCFC
        C3CCDFFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFBFBFBFBFBFBFAFAFAFBFB
        FBBFBFBFFF00FFFF00FFFF00FFFF00FF58B9D87BCBE37CCDE47ECDE47FD0E566
        A7B9BFBFBFE9D0BAC3CCDFE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BA
        E9D0BAE9D0BAE9D0BABFBFBFFF00FFFF00FFFF00FFFF00FF59BCDA7DCDE37FCE
        E57FD1E681D1E867A7B9BFBFBFFDFDFDC3CCDFFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFCFCFCEAEAEAE4E4E4ECECECBFBFBFFF00FFFF00FFFF00FFFF00FF
        5BBEDC7ED0E67FD0E780D2E782D2E869AABBBFBFBFE9D0BAC3CCDFE9D0BAE9D0
        BAE9D0BAE9D0BAE9D0BAE9D0BAE9D0BABFBFBFBFBFBFBFBFBFBFBFBFFF00FFFF
        00FFFF00FFFF00FF5CC0DE80D0E781D3E882D3EA82D5E96AABBDBFBFBF6C6C6C
        C3CCDFFDFDFDFEFEFEFEFEFEFEFEFEFDFDFDFDFDFDFBFBFBBFBFBFF1D5F1BFBF
        BFFF00FFFF00FFFF00FFFF00FFFF00FF5DC2DF83D4EA82D3E9C0BBBBC0BBBBC0
        BBBBC0BBBBC0BBBBC0BBBBC0BBBBC0BBBBC0BBBBC0BBBBC0BBBBF9F9F9FCFCFC
        BFBFBFBFBFBFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF5FC4E283D4EA85D6
        EC9FCCD7C0BBBBC0BBBBC0BBBBECEBEBECEBEBECEBEBC0BBBBC0BBBBC0BBBBBF
        BDBDC0C0C0BFBFBFBFBFBFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        5FC6E385D7ED85D9ED86DAEEA0A0A0A0A0A0C0BBBBE0DDDDE0DDDDE0DDDDC0BB
        BB9F9F9FA0A0A090EAF892EAFA93ECFA5BBDDBFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FF60C6E486D9EE87DCEF88DDEFC5C3C3CFCDCDC0BBBBD4D0D0
        DAD9D9D4D0D0C0BBBBCDCBCBC4C4C492EAFA94EDFB94EDFC5BC0DEFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FF60C7E460C7E460C7E460C7E4E6E6E6E6
        E6E6E5E4E4C0BBBBD0CDCDC0BBBBE1E0E0E6E6E6E6E6E65EC4E15DC2E05EC2E0
        5CC2E0FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFC0BBBBFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      TabOrder = 9
      OnClick = btnPasteSettingsClick
    end
  end
  object scrollBoxPreview: TScrollBox
    Left = 259
    Top = 0
    Width = 462
    Height = 450
    Align = alClient
    TabOrder = 1
    object imagePreview: TImage
      Left = 4
      Top = -1
      Width = 105
      Height = 105
      OnDblClick = imagePreviewDblClick
      OnMouseDown = imagePreviewMouseDown
      OnMouseUp = imagePreviewMouseUp
    end
  end
  object fdTextFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 264
    Top = 40
  end
  object spdSaveImage: TSavePictureDialog
    DefaultExt = '.emf'
    Filter = 
      'Enhanced Metafile with included model bitmap (*.emf)|*.emf|Bitma' +
      'p (*.bmp)|*.bmp|Portable Network Graphic (*.png)|*.png|Joint Pho' +
      'tographic Experts Group image (*.jpg)|*.jpg|Pure vector graphic ' +
      '(*.emf)|*.emf'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    OnTypeChange = spdSaveImageTypeChange
    Left = 304
    Top = 40
  end
  object pdPrintImage: TPrintDialog
    Left = 264
    Top = 88
  end
  object timerDrawImageDelay: TTimer
    Enabled = False
    Interval = 100
    OnTimer = timerDrawImageDelayTimer
    Left = 200
    Top = 64
  end
  object pmChangeStates: TPopupMenu
    Left = 304
    Top = 88
    object miCheckSelected: TMenuItem
      Caption = 'Check Selected'
      Hint = 'Check the selected items'
      OnClick = miCheckSelectedClick
    end
    object UncheckSelected1: TMenuItem
      Caption = 'Uncheck Selected'
      Hint = 'Uncheck the selected items'
      OnClick = UncheckSelected1Click
    end
  end
end
