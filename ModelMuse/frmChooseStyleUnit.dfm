inherited frmChooseStyle: TfrmChooseStyle
  Left = 395
  Top = 485
  Width = 732
  Height = 338
  HelpKeyword = 'ChooseStyleFrame'
  HorzScrollBar.Range = 0
  VertScrollBar.Range = 0
  ActiveControl = rgStyle
  AutoScroll = False
  Caption = 'Choose Style'
  Menu = MainMenu1
  PixelsPerInch = 96
  TextHeight = 17
  object rgStyle: TRadioGroup
    Left = 0
    Top = 0
    Width = 210
    Height = 231
    Align = alLeft
    Caption = 'Style'
    Items.Strings = (
      'Windows'
      'Motif'
      'Motif Plus'
      'CDE'
      'Qt SGI'
      'Platinum'
      'System default')
    TabOrder = 0
    OnClick = rgStyleClick
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 231
    Width = 724
    Height = 57
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    object lblInstructions: TLabel
      Left = 8
      Top = 4
      Width = 335
      Height = 34
      Caption = 
        'Select one of the styles on the left and see how it will look in' +
        ' the Preview section on the right.'
      WordWrap = True
    end
    object btnClose: TBitBtn
      Left = 632
      Top = 8
      Width = 91
      Height = 33
      TabOrder = 1
      Kind = bkClose
    end
    object btnHelp: TBitBtn
      Left = 536
      Top = 8
      Width = 91
      Height = 33
      TabOrder = 0
      Kind = bkHelp
    end
  end
  object gbPreview: TGroupBox
    Left = 210
    Top = 0
    Width = 514
    Height = 231
    Align = alClient
    Caption = 'Preview'
    TabOrder = 2
    object cbUnchecked: TCheckBox
      Left = 16
      Top = 48
      Width = 153
      Height = 31
      Caption = 'Unchecked'
      TabOrder = 1
    end
    object comboPreview: TComboBox
      Left = 16
      Top = 136
      Width = 153
      Height = 25
      ItemHeight = 17
      TabOrder = 4
      Text = 'Combo Box'
      Items.Strings = (
        'Choice 1'
        'Choice 2'
        'Choice 3')
    end
    object cbChecked: TCheckBox
      Left = 16
      Top = 24
      Width = 153
      Height = 31
      Caption = 'Checked'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object edPreview: TEdit
      Left = 16
      Top = 176
      Width = 153
      Height = 25
      Cursor = crIBeam
      TabOrder = 5
      Text = 'Edit Box'
    end
    object cbIndeterminant: TCheckBox
      Left = 16
      Top = 72
      Width = 153
      Height = 31
      AllowGrayed = True
      Caption = 'Indeterminant'
      State = cbGrayed
      TabOrder = 2
    end
    object memoPreview: TMemo
      Left = 176
      Top = 24
      Width = 329
      Height = 89
      Lines.Strings = (
        'Text')
      TabOrder = 7
    end
    object btnPreview: TButton
      Left = 16
      Top = 216
      Width = 153
      Height = 25
      Caption = 'Do nothing button'
      TabOrder = 6
      OnClick = btnPreviewClick
    end
    object cbDisabled: TCheckBox
      Left = 16
      Top = 96
      Width = 101
      Height = 31
      Caption = 'Disabled'
      Enabled = False
      TabOrder = 3
    end
    object RbwDataGrid41: TRbwDataGrid4
      Left = 176
      Top = 120
      Width = 329
      Height = 105
      ColCount = 3
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
      TabOrder = 8
      AutoDistributeText = False
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      ColorSelectedRow = True
      Columns = <
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'MS Sans Serif'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'MS Sans Serif'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          Format = rcf4Boolean
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'MS Sans Serif'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          Format = rcf4Combo
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          PickList.Strings = (
            #39'Choice 1'
            'Choice 2'
            'Choice 3')
          WordWrapCaptions = False
          WordWrapCells = False
          AutoAdjustColWidths = True
        end>
      RowHeights = (
        24
        24
        24
        24
        24)
    end
  end
  object MainMenu1: TMainMenu
    Left = 138
    Top = 8
    object menuPreview1: TMenuItem
      Caption = 'Preview'
      object menuPreview2: TMenuItem
        Caption = 'This menu is just to give you a preview of what menus look like'
        OnClick = menuPreview2Click
      end
    end
  end
end
