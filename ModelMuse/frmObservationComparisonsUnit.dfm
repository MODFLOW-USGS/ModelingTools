inherited frmObservationComparisons: TfrmObservationComparisons
  Caption = 'Observation Comparisons'
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 191
    Width = 424
    Height = 35
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      424
      35)
    object btnCancel: TBitBtn
      Left = 336
      Top = 2
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 248
      Top = 2
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
    end
    object btnHelp: TBitBtn
      Left = 160
      Top = 2
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
  end
  object treecomboInPlaceEditor: TRbwStringTreeCombo
    Left = 48
    Top = 24
    Width = 121
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
    Tree.OnFreeNode = treecomboInPlaceEditorTreeFreeNode
    Tree.OnGetText = treecomboInPlaceEditorTreeGetText
    Tree.OnGetNodeDataSize = treecomboInPlaceEditorTreeGetNodeDataSize
    Tree.OnInitNode = treecomboInPlaceEditorTreeInitNode
    Tree.ExplicitWidth = 200
    Tree.ExplicitHeight = 100
    Tree.Columns = <>
    Enabled = True
    Glyph.Data = {
      36020000424D3602000000000000360000002800000010000000080000000100
      2000000000000002000000000000000000000000000000000000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F00000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000C0C0C000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000000000000000000000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000C0C0C000C0C0C000C0C0C000F0F0F000F0F0F000F0F0F000F0F0F0000000
      000000000000000000000000000000000000F0F0F000F0F0F000F0F0F000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000F0F0F000F0F0F000000000000000
      00000000000000000000000000000000000000000000F0F0F000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000}
    NumGlyphs = 2
    TabOrder = 1
  end
end
