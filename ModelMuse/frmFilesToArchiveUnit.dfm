inherited frmFilesToArchive: TfrmFilesToArchive
  HelpType = htKeyword
  HelpKeyword = 'Files_to_Archive_Dialog_Box'
  Caption = ' Files To Archive'
  ClientWidth = 631
  ExplicitWidth = 647
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 153
    Width = 631
    Height = 73
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      631
      73)
    object JvLinkLabel1: TJvLinkLabel
      Left = 8
      Top = 6
      Width = 297
      Height = 18
      Caption = '<link>USGS model archiving policy<\link>'
      Text.Strings = (
        '<link>USGS model archiving policy<\link>')
      OnLinkClick = JvLinkLabel1LinkClick
    end
    object btnCancel: TBitBtn
      Left = 533
      Top = 30
      Width = 83
      Height = 32
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 5
    end
    object btnOK: TBitBtn
      Left = 444
      Top = 30
      Width = 83
      Height = 32
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 357
      Top = 30
      Width = 81
      Height = 32
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
    end
    object btnArchive: TButton
      Left = 8
      Top = 30
      Width = 121
      Height = 32
      Caption = 'Create archive'
      TabOrder = 0
      OnClick = btnArchiveClick
    end
    object btnAddFiles: TButton
      Left = 135
      Top = 30
      Width = 75
      Height = 32
      Caption = 'Add files'
      TabOrder = 1
      OnClick = mniAddFilesClick
    end
    object btnArchiveList: TButton
      Left = 216
      Top = 30
      Width = 129
      Height = 32
      Caption = 'Save archive list'
      TabOrder = 2
      OnClick = btnArchiveListClick
    end
  end
  object tvArchive: TTreeView
    Left = 0
    Top = 0
    Width = 631
    Height = 153
    Align = alClient
    DragMode = dmAutomatic
    Indent = 19
    MultiSelect = True
    PopupMenu = pm1
    ReadOnly = True
    TabOrder = 0
    OnCustomDrawItem = tvArchiveCustomDrawItem
    OnDragDrop = tvArchiveDragDrop
    OnDragOver = tvArchiveDragOver
  end
  object sdArchive: TSaveDialog
    DefaultExt = '.zip'
    Filter = 
      'Archives (*.zip;*.tgz;*.tar;*.cab;*.tbz)|*.zip;*.tgz;*.tar;*.cab' +
      ';*.tbz'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 24
    Top = 16
  end
  object odAddFiles: TOpenDialog
    Filter = 'Any File (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 88
    Top = 72
  end
  object dlgSaveArchiveList: TSaveDialog
    DefaultExt = '.axml'
    Filter = 'Archive Lists (*.axml)|*.axml'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 152
    Top = 40
  end
  object pm1: TPopupMenu
    Left = 280
    Top = 88
    object mniAddFiles: TMenuItem
      Caption = 'Add Files'
      OnClick = mniAddFilesClick
    end
    object mniDelete: TMenuItem
      Caption = 'Delete'
      OnClick = mniDeleteClick
    end
  end
end
