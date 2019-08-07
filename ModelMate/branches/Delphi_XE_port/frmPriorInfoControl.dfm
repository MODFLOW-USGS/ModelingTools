object FormPriorInfoControl: TFormPriorInfoControl
  Left = 0
  Top = 0
  Caption = 'Prior Information Control'
  ClientHeight = 430
  ClientWidth = 598
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    598
    430)
  PixelsPerInch = 96
  TextHeight = 16
  object Label12: TLabel
    Left = 136
    Top = 10
    Width = 429
    Height = 32
    Caption = 
      'Defined prior information can be omitted or included using this ' +
      'check box.  Clear the check box to omit all defined prior inform' +
      'ation.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 52
    Top = 57
    Width = 192
    Height = 16
    Caption = 'Summary of Prior Information'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblNumPri: TLabel
    Left = 115
    Top = 81
    Width = 196
    Height = 16
    Caption = 'Number of prior-information items'
  end
  object lblNumPriGps: TLabel
    Left = 115
    Top = 105
    Width = 204
    Height = 16
    Caption = 'Number of prior-information groups'
  end
  object cbUsePriorInfo: TJvCheckBox
    Left = 9
    Top = 10
    Width = 121
    Height = 34
    Caption = 'Use Prior Information'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    WordWrap = True
    OnClick = cbUsePriorInfoClick
    LinkedControls = <>
    AutoSize = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'Tahoma'
    HotTrackFont.Style = []
  end
  object jvmNumPri: TJvMemo
    Left = 9
    Top = 80
    Width = 100
    Height = 18
    TabStop = False
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Alignment = taRightJustify
    BorderStyle = bsNone
    Enabled = False
    Flat = True
    Lines.Strings = (
      'jvmNumPri')
    ParentFlat = False
    ReadOnly = True
    TabOrder = 3
  end
  object jvmNumPriGps: TJvMemo
    Left = 9
    Top = 104
    Width = 100
    Height = 18
    TabStop = False
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Alignment = taRightJustify
    BorderStyle = bsNone
    Enabled = False
    Flat = True
    Lines.Strings = (
      'jvmNumPriGps')
    ParentFlat = False
    ReadOnly = True
    TabOrder = 4
  end
  object dgPriGpSummary: TDataGrid
    Left = 9
    Top = 130
    Width = 277
    Height = 295
    TabStop = False
    Anchors = [akLeft, akTop, akBottom]
    ColCount = 2
    Ctl3D = False
    DefaultRowHeight = 23
    Enabled = False
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    ParentCtl3D = False
    ScrollBars = ssVertical
    TabOrder = 5
    Columns = <
      item
        ReadOnly = True
        Title.Caption = 'Group name'
        Title.WordWrap = False
      end
      item
        ReadOnly = True
        Title.Caption = 'Number of prior info items'
        Title.WordWrap = False
      end>
    RowCountMin = 0
    SelectedIndex = 0
    Version = '2.0'
    ColWidths = (
      105
      170)
  end
  object btnPriorForm: TButton
    Left = 321
    Top = 130
    Width = 224
    Height = 60
    Caption = 'Define Linear Prior Information...'
    TabOrder = 1
    OnClick = btnPriorFormClick
  end
  object btnClose: TButton
    Left = 396
    Top = 290
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnCloseClick
  end
end
