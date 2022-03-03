object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Video Links'
  ClientHeight = 367
  ClientWidth = 638
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 121
    Top = 0
    Width = 5
    Height = 348
    ExplicitHeight = 724
  end
  object tvVideos: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 348
    Align = alLeft
    AutoExpand = True
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnChange = tvVideosChange
    OnCompare = tvVideosCompare
    OnMouseMove = tvVideosMouseMove
    ExplicitLeft = -1
    ExplicitTop = -6
  end
  object sbStatus: TStatusBar
    Left = 0
    Top = 348
    Width = 638
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitTop = 302
    ExplicitWidth = 635
  end
  object Panel1: TPanel
    Left = 126
    Top = 0
    Width = 512
    Height = 348
    Align = alClient
    TabOrder = 2
    ExplicitLeft = 136
    ExplicitTop = 304
    ExplicitWidth = 937
    ExplicitHeight = 353
    object lblDate: TLabel
      Left = 6
      Top = 197
      Width = 61
      Height = 13
      Caption = 'Posting Date'
    end
    object lblTitle: TLabel
      Left = 6
      Top = 150
      Width = 20
      Height = 13
      Caption = 'Title'
    end
    object lblTopic: TLabel
      Left = 7
      Top = 248
      Width = 57
      Height = 13
      Caption = 'Topic Group'
    end
    object lblURL: TLabel
      Left = 6
      Top = 104
      Width = 19
      Height = 13
      Caption = 'URL'
    end
    object btnAddChild: TButton
      Left = 6
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Add Child'
      TabOrder = 0
      OnClick = btnAddChildClick
    end
    object btnAddNode: TButton
      Left = 6
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Add Node'
      TabOrder = 1
      OnClick = btnAddNodeClick
    end
    object btnDeleteNode: TButton
      Left = 7
      Top = 71
      Width = 75
      Height = 25
      Caption = 'Delete Node'
      TabOrder = 2
      OnClick = btnDeleteNodeClick
    end
    object edTitle: TEdit
      Left = 6
      Top = 169
      Width = 492
      Height = 21
      TabOrder = 3
      OnChange = edTitleChange
    end
    object edURL: TEdit
      Left = 6
      Top = 123
      Width = 492
      Height = 21
      TabOrder = 4
      OnChange = edURLChange
    end
    object jvdDate: TJvDateEdit
      Left = 6
      Top = 216
      Width = 121
      Height = 21
      ShowNullDate = False
      TabOrder = 5
      OnChange = jvdDateChange
    end
    object comtoTitleGroup: TComboBox
      Left = 6
      Top = 267
      Width = 279
      Height = 21
      TabOrder = 6
      OnChange = comtoTitleGroupChange
      OnExit = comtoTitleGroupExit
    end
    object btnSortByTitle: TButton
      Left = 136
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Sort by Title'
      TabOrder = 7
      OnClick = btnSortByTitleClick
    end
    object btnSortByGroup: TButton
      Left = 136
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Sort by Group'
      TabOrder = 8
      OnClick = btnSortByGroupClick
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '.xml'
    Filter = 'Xml (*.xml)|*.xml'
    Left = 312
    Top = 80
  end
  object dlgSave: TSaveDialog
    DefaultExt = '.xml'
    Filter = 'Xml (*.xml)|*.xml'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 368
    Top = 88
  end
  object MainMenu1: TMainMenu
    Left = 248
    Top = 48
    object File1: TMenuItem
      Caption = 'File'
      object miOpen: TMenuItem
        Caption = 'Open'
        OnClick = miOpenClick
      end
      object miSave: TMenuItem
        Caption = 'Save'
        OnClick = miSaveClick
      end
    end
  end
end
