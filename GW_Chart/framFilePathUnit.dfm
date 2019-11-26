object framFilePath: TframFilePath
  Left = 0
  Top = 0
  Width = 485
  Height = 49
  TabOrder = 0
  DesignSize = (
    485
    49)
  object lblFileType: TLabel
    Left = 2
    Top = 3
    Width = 43
    Height = 13
    Caption = 'File Type'
  end
  object edFilePath: TEdit
    Left = 0
    Top = 21
    Width = 401
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clRed
    TabOrder = 0
    Text = 'File Path'
    OnChange = edFilePathChange
  end
  object btnBrowse: TButton
    Left = 408
    Top = 21
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object OpenDialogPath: TOpenDialog
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 64
    Top = 16
  end
end
