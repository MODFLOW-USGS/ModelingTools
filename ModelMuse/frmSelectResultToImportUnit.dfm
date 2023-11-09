inherited frmSelectResultToImport: TfrmSelectResultToImport
  HelpType = htKeyword
  HelpKeyword = 'Select_Results_to_Import_Dialog_Box'
  Caption = ' Select Model Results to Import'
  ClientHeight = 457
  ClientWidth = 627
  ExplicitWidth = 639
  ExplicitHeight = 495
  TextHeight = 18
  object lblColorGrid: TLabel
    Left = 8
    Top = 350
    Width = 231
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Data used to color or contour grid'
    ExplicitTop = 355
  end
  object lblClassification: TLabel
    Left = 224
    Top = 277
    Width = 94
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Classification'
    ExplicitTop = 282
  end
  object lblPrefix: TLabel
    Left = 496
    Top = 275
    Width = 40
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Prefix'
    ExplicitTop = 280
  end
  object btnHelp: TBitBtn
    Left = 299
    Top = 402
    Width = 91
    Height = 47
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 7
    OnClick = btnHelpClick
    ExplicitLeft = 295
    ExplicitTop = 401
  end
  object btnOK: TBitBtn
    Left = 396
    Top = 402
    Width = 91
    Height = 47
    Anchors = [akRight, akBottom]
    Enabled = False
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 8
    OnClick = btnOKClick
    ExplicitLeft = 392
    ExplicitTop = 401
  end
  object btnCancel: TBitBtn
    Left = 493
    Top = 402
    Width = 91
    Height = 47
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 9
    ExplicitLeft = 489
    ExplicitTop = 401
  end
  object comboColorGrid: TComboBox
    Left = 8
    Top = 370
    Width = 575
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
    ExplicitTop = 369
    ExplicitWidth = 571
  end
  object btnSelectAll: TButton
    Left = 8
    Top = 402
    Width = 91
    Height = 47
    Anchors = [akLeft, akBottom]
    Caption = 'Select all data sets'
    TabOrder = 5
    WordWrap = True
    OnClick = btnSelectAllClick
    ExplicitTop = 401
  end
  object btnSelectNone: TButton
    Left = 105
    Top = 402
    Width = 91
    Height = 47
    Anchors = [akLeft, akBottom]
    Caption = 'Deselect all data sets'
    TabOrder = 6
    WordWrap = True
    OnClick = btnSelectNoneClick
    ExplicitTop = 401
  end
  object rgDisplayChoice: TRadioGroup
    Left = 8
    Top = 302
    Width = 575
    Height = 42
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Display choice'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Color grid'
      'Contour grid'
      'Neither')
    TabOrder = 3
    ExplicitTop = 301
    ExplicitWidth = 571
  end
  object pnlSelections: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 621
    Height = 265
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 617
    ExplicitHeight = 264
    object splitData: TSplitter
      Left = 0
      Top = 125
      Width = 621
      Height = 5
      Cursor = crVSplit
      Align = alTop
      ExplicitLeft = 1
      ExplicitTop = 105
      ExplicitWidth = 510
    end
    object splMultiSelect: TSplitter
      Left = 343
      Top = 130
      Width = 5
      Height = 135
      Align = alRight
      ExplicitLeft = 318
      ExplicitHeight = 169
    end
    object rdgModels: TRbwDataGrid4
      Left = 0
      Top = 0
      Width = 621
      Height = 125
      Align = alTop
      ColCount = 3
      DefaultColWidth = 20
      FixedCols = 1
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
      TabOrder = 0
      ExtendedAutoDistributeText = False
      AutoMultiEdit = True
      AutoDistributeText = False
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = False
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnBeforeDrawCell = rdgModelsBeforeDrawCell
      OnButtonClick = rdgModelsButtonClick
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
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = 'Browse'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
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
          WordWrapCells = True
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      WordWrapRowCaptions = False
      ExplicitWidth = 617
      ColWidths = (
        20
        20
        313)
      RowHeights = (
        24
        24)
    end
    object clData: TJvCheckListBox
      Left = 0
      Top = 130
      Width = 343
      Height = 135
      Align = alClient
      DoubleBuffered = False
      ItemHeight = 18
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClickCheck = clDataClickCheck
      ExplicitWidth = 339
      ExplicitHeight = 134
    end
    object pnlMultiSelect: TPanel
      Left = 348
      Top = 130
      Width = 273
      Height = 135
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 344
      ExplicitHeight = 134
      object spl1: TSplitter
        Left = 0
        Top = 81
        Width = 273
        Height = 5
        Cursor = crVSplit
        Align = alTop
        ExplicitWidth = 185
      end
      object clTime: TJvxCheckListBox
        Left = 0
        Top = 86
        Width = 273
        Height = 49
        Align = alClient
        ItemHeight = 18
        TabOrder = 1
        OnStateChange = clTimeStateChange
        ExplicitHeight = 48
        InternalVersion = 202
      end
      object clDescription: TJvxCheckListBox
        Left = 0
        Top = 0
        Width = 273
        Height = 81
        Align = alTop
        ItemHeight = 18
        TabOrder = 0
        OnStateChange = clDescriptionStateChange
        InternalVersion = 202
      end
    end
  end
  object comboClassification: TComboBox
    Left = 8
    Top = 274
    Width = 210
    Height = 26
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    OnChange = comboClassificationChange
    ExplicitTop = 273
  end
  object edPrefix: TRbwEdit
    Left = 355
    Top = 274
    Width = 121
    Height = 26
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 2
    ExplicitTop = 273
  end
  object odSelectFiles: TJvOpenDialog
    Filter = 
      'All supported file types|*.bhd;*.bdn;*.fhd;*.fdn;*.cbc;*.huf_fhd' +
      ';*.huf_bhd;*.huf_flow;*.Sub_Out;*.SubSubOut;*.SubComMlOut;*.SubC' +
      'omIsOut;*.SubVdOut;*.SubNdCritHeadOut;*.SubDCritHeadOut|Formatte' +
      'd head files (*.fhd)|*.fhd|Formatted drawdown files (*.fdn)|*.fd' +
      'n|Binary head files (*.bhd)|*.bhd|Binary drawdown files (*.bdn)|' +
      '*.bdn|Binary flow files (*.cbc)|*.cbc|Formatted HUF head files (' +
      '*.huf_fhd)|*.huf_fhd|Binary HUF head files (*.huf_bhd)|*.huf_bhd' +
      '|HUF flow files (*.huf_flow)|*.huf_flow|Combined SUB output file' +
      ' (*.Sub_Out)|*.Sub_Out|Subsidence (*.SubSubOut)|*.SubSubOut|Comp' +
      'action by model layer (*.SubComMlOut)|*.SubComMlOut|Compaction b' +
      'y interbed system (*.SubComIsOut)|*.SubComIsOut|Vertical displac' +
      'ement (*.SubVdOut)|*.SubVdOut|Critical head for no-delay interbe' +
      'ds (*.SubNdCritHeadOut)|*.SubNdCritHeadOut|Critical head for del' +
      'ay interbeds (*.SubDCritHeadOut)|*.SubDCritHeadOut'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select Model File'
    OnTypeChange = odSelectFilesTypeChange
    Height = 454
    Width = 563
    Left = 8
    Top = 248
  end
end
