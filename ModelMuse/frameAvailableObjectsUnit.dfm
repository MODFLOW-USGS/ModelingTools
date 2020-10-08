object frameAvailableObjects: TframeAvailableObjects
  Left = 0
  Top = 0
  Width = 389
  Height = 197
  TabOrder = 0
  OnResize = FrameResize
  object lblSrcObjects: TLabel
    Left = 8
    Top = 4
    Width = 81
    Height = 13
    Caption = 'Available objects'
  end
  object lblDstObjects: TLabel
    Left = 215
    Top = 3
    Width = 62
    Height = 13
    Caption = 'Used objects'
  end
  object lbSrcObjects: TJvListBox
    Left = 3
    Top = 23
    Width = 164
    Height = 160
    DragMode = dmAutomatic
    ItemHeight = 13
    Background.FillMode = bfmTile
    Background.Visible = False
    MultiSelect = True
    ParentShowHint = False
    ShowHint = True
    Sorted = True
    TabOrder = 0
    OnClick = lbSrcObjectsClick
  end
  object lbDstObjects: TJvListBox
    Left = 216
    Top = 22
    Width = 164
    Height = 160
    DragMode = dmAutomatic
    ItemHeight = 13
    Background.FillMode = bfmTile
    Background.Visible = False
    MultiSelect = True
    ParentShowHint = False
    ShowHint = True
    Sorted = True
    TabOrder = 1
    OnClick = lbDstObjectsClick
  end
  object btnIncObjects: TButton
    Left = 184
    Top = 59
    Width = 26
    Height = 26
    Hint = 'Move selected objects into "Used objects"'
    Caption = '>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = btnIncObjectsClick
  end
  object btnIncAllObjects: TButton
    Left = 184
    Top = 91
    Width = 26
    Height = 26
    Hint = 'Move all objects into "Used objects"'
    Caption = '>>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = btnIncAllObjectsClick
  end
  object btnExclObjects: TButton
    Left = 184
    Top = 124
    Width = 26
    Height = 26
    Hint = 'Move selected objects out of "Used objects"'
    Caption = '<'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = btnExclObjectsClick
  end
  object btnExclAllObjects: TButton
    Left = 184
    Top = 156
    Width = 26
    Height = 26
    Hint = 'Move all objects out of "Used objects"'
    Caption = '<<'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = btnExclAllObjectsClick
  end
end
