object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 321
  ClientWidth = 635
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
  DesignSize = (
    635
    321)
  PixelsPerInch = 96
  TextHeight = 13
  object lblURL: TLabel
    Left = 135
    Top = 11
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object lblTitle: TLabel
    Left = 135
    Top = 150
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object lblDate: TLabel
    Left = 135
    Top = 197
    Width = 61
    Height = 13
    Caption = 'Posting Date'
  end
  object lblTopic: TLabel
    Left = 136
    Top = 248
    Width = 57
    Height = 13
    Caption = 'Topic Group'
  end
  object tvVideos: TTreeView
    Left = 9
    Top = 8
    Width = 121
    Height = 283
    Anchors = [akLeft, akTop, akBottom]
    AutoExpand = True
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnChange = tvVideosChange
    OnMouseMove = tvVideosMouseMove
  end
  object edURL: TEdit
    Left = 135
    Top = 30
    Width = 492
    Height = 21
    TabOrder = 1
    OnChange = edURLChange
  end
  object btnAddChild: TButton
    Left = 135
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Add Child'
    TabOrder = 2
    OnClick = btnAddChildClick
  end
  object btnDeleteNode: TButton
    Left = 136
    Top = 119
    Width = 75
    Height = 25
    Caption = 'Delete Node'
    TabOrder = 3
    OnClick = btnDeleteNodeClick
  end
  object btnAddNode: TButton
    Left = 135
    Top = 57
    Width = 75
    Height = 25
    Caption = 'Add Node'
    TabOrder = 4
    OnClick = btnAddNodeClick
  end
  object edTitle: TEdit
    Left = 135
    Top = 169
    Width = 492
    Height = 21
    TabOrder = 5
    OnChange = edTitleChange
  end
  object sbStatus: TStatusBar
    Left = 0
    Top = 302
    Width = 635
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object jvdDate: TJvDateEdit
    Left = 135
    Top = 216
    Width = 121
    Height = 21
    ShowNullDate = False
    TabOrder = 7
    OnChange = jvdDateChange
  end
  object edTopic: TEdit
    Left = 135
    Top = 267
    Width = 281
    Height = 21
    TabOrder = 8
    OnChange = edTopicChange
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
end
