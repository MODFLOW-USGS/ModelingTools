object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Video Links'
  ClientHeight = 353
  ClientWidth = 709
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
    Left = 189
    Top = 0
    Width = 5
    Height = 334
    Align = alRight
    ExplicitLeft = 642
  end
  object tvVideos: TTreeView
    Left = 0
    Top = 0
    Width = 189
    Height = 334
    Align = alClient
    AutoExpand = True
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnChange = tvVideosChange
    OnCompare = tvVideosCompare
    OnMouseMove = tvVideosMouseMove
  end
  object sbStatus: TStatusBar
    Left = 0
    Top = 334
    Width = 709
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Panel1: TPanel
    Left = 194
    Top = 0
    Width = 515
    Height = 334
    Align = alRight
    TabOrder = 2
    DesignSize = (
      515
      334)
    object lblDate: TLabel
      Left = 6
      Top = 197
      Width = 61
      Height = 13
      Caption = 'Posting Date'
    end
    object lblTitle: TLabel
      Left = 6
      Top = 105
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
      Top = 151
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
      Top = 124
      Width = 499
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edTitleChange
    end
    object edURL: TEdit
      Left = 6
      Top = 170
      Width = 499
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      TextHint = 'URL'
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
    object cbUpdated: TCheckBox
      Left = 6
      Top = 294
      Width = 97
      Height = 17
      Caption = 'Updated'
      TabOrder = 9
      OnClick = cbUpdatedClick
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
