inherited frmDataSetValues: TfrmDataSetValues
  HelpType = htKeyword
  HelpKeyword = 'Data_Set_Values_Dialog_Box'
  Caption = 'Data Set Values'
  ClientHeight = 304
  ClientWidth = 548
  ExplicitWidth = 564
  ExplicitHeight = 343
  PixelsPerInch = 96
  TextHeight = 18
  object Panel1: TPanel
    Left = 0
    Top = 160
    Width = 548
    Height = 144
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      548
      144)
    object lblDataSet: TLabel
      Left = 4
      Top = 78
      Width = 59
      Height = 18
      Caption = 'Data set'
    end
    object lblModel: TLabel
      Left = 4
      Top = 46
      Width = 43
      Height = 18
      Caption = 'Model'
    end
    object btnClose: TBitBtn
      Left = 459
      Top = 108
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 6
    end
    object btnHelp: TBitBtn
      Left = 459
      Top = 76
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnHelpClick
    end
    object btnCopy: TButton
      Left = 459
      Top = 45
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Copy'
      Enabled = False
      TabOrder = 2
      OnClick = btnCopyClick
    end
    object comboModel: TComboBox
      Left = 81
      Top = 43
      Width = 356
      Height = 26
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = comboModelChange
    end
    object treecomboDataSets: TRbwStringTreeCombo
      Left = 81
      Top = 75
      Width = 356
      Height = 26
      Tree.Left = 0
      Tree.Top = 0
      Tree.Width = 304
      Tree.Height = 201
      Tree.Align = alClient
      Tree.DefaultNodeHeight = 20
      Tree.Header.AutoSizeIndex = 0
      Tree.Header.MainColumn = -1
      Tree.TabOrder = 0
      Tree.TreeOptions.SelectionOptions = [toFullRowSelect]
      Tree.OnChange = treecomboDataSetsDropDownTreeChange
      Tree.OnGetText = treecomboDataSetsDropDownTreeGetText
      Tree.OnGetNodeDataSize = treecomboDataSetsDropDownTreeGetNodeDataSize
      Tree.OnInitNode = treecomboDataSets1TreeInitNode
      Tree.Touch.InteractiveGestures = [igPan, igPressAndTap]
      Tree.Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Tree.Columns = <>
      Anchors = [akLeft, akTop, akRight]
      Enabled = True
      Glyph.Data = {
        36020000424D3602000000000000360000002800000010000000080000000100
        2000000000000002000000000000000000000000000000000000D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC0000000000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00C0C0C000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00000000000000000000000000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00D8E9EC00D8E9EC000000
        000000000000000000000000000000000000D8E9EC00D8E9EC00D8E9EC00C0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00000000000000
        00000000000000000000000000000000000000000000D8E9EC00C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00}
      NumGlyphs = 2
      TabOrder = 3
      OnChange = treecomboDataSetsChange
    end
    object comboOrientation: TComboBox
      Left = 81
      Top = 107
      Width = 356
      Height = 26
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 5
      Text = 'Layer'
      Visible = False
      OnChange = comboOrientationChange
      Items.Strings = (
        'Layer'
        'Row'
        'Column')
    end
    object btnSave: TButton
      Left = 459
      Top = 13
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Enabled = False
      TabOrder = 0
      OnClick = btnSaveClick
    end
    object cbIncludeCaptions: TCheckBox
      Left = 168
      Top = 15
      Width = 257
      Height = 21
      Caption = 'Copy row and column captions'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
  end
  object pcDataSet: TPageControl
    Left = 97
    Top = 0
    Width = 451
    Height = 160
    Align = alClient
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 97
    Height = 160
    Align = alLeft
    Caption = 'Panel2'
    TabOrder = 0
    object lblLayer: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 89
      Height = 18
      Align = alTop
      Alignment = taCenter
      Caption = 'Layer'
      ExplicitWidth = 39
    end
    object lbLayers: TJvListBox
      Left = 1
      Top = 25
      Width = 95
      Height = 134
      Align = alClient
      ItemHeight = 18
      Background.FillMode = bfmTile
      Background.Visible = False
      TabOrder = 0
      OnMouseUp = lbLayersMouseUp
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text files *.txt|*.txt|All Files (*.*)|*.*'
    Left = 264
    Top = 16
  end
end
