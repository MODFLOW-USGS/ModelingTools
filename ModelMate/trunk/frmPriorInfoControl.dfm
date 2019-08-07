object FormPriorInfoControl: TFormPriorInfoControl
  Left = 0
  Top = 0
  Caption = 'Prior-Information Control'
  ClientHeight = 411
  ClientWidth = 681
  Color = clBtnFace
  Constraints.MaxWidth = 818
  Constraints.MinHeight = 428
  Constraints.MinWidth = 689
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    681
    411)
  PixelsPerInch = 120
  TextHeight = 18
  object Label12: TLabel
    Left = 176
    Top = 9
    Width = 477
    Height = 72
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'Defined prior information can be omitted or included using this ' +
      'check box.  Clear the check box to omit all defined prior inform' +
      'ation.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 62
    Top = 89
    Width = 232
    Height = 19
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Summary of Prior Information'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblNumPri: TLabel
    Left = 137
    Top = 117
    Width = 232
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Number of prior-information items'
  end
  object lblNumPriGps: TLabel
    Left = 137
    Top = 146
    Width = 242
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Number of prior-information groups'
  end
  object cbUsePriorInfo: TJvCheckBox
    Left = 11
    Top = 12
    Width = 157
    Height = 58
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Use Prior Information'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    WordWrap = True
    OnClick = cbUsePriorInfoClick
    LinkedControls = <>
    AutoSize = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -17
    HotTrackFont.Name = 'Arial'
    HotTrackFont.Style = []
  end
  object jvmNumPri: TJvMemo
    Left = 11
    Top = 116
    Width = 118
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabStop = False
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
    Left = 11
    Top = 145
    Width = 118
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabStop = False
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
  object dgPriGpSummary: TEcDataGrid
    Left = 11
    Top = 184
    Width = 373
    Height = 221
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
      131
      264)
  end
  object btnPriorForm: TButton
    Left = 392
    Top = 184
    Width = 284
    Height = 72
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Define Linear Prior Information...'
    TabOrder = 1
    OnClick = btnPriorFormClick
  end
  object btnClose: TButton
    Left = 486
    Top = 360
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnCloseClick
  end
end
