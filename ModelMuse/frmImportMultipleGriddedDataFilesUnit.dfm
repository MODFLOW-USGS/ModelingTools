inherited frmImportMultipleGriddedDataFiles: TfrmImportMultipleGriddedDataFiles
  HelpType = htKeyword
  HelpKeyword = 'Import_Gridded_Data_Files_Dial'
  Caption = 'Import Gridded Data Files'
  ClientHeight = 267
  ExplicitWidth = 438
  ExplicitHeight = 305
  TextHeight = 18
  inline frameGridFiles: TframeGrid
    Left = 0
    Top = 0
    Width = 426
    Height = 132
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 422
    ExplicitHeight = 131
    inherited Panel: TPanel
      Top = 91
      Width = 426
      ExplicitTop = 90
      ExplicitWidth = 422
      inherited lbNumber: TLabel
        Width = 55
        Height = 18
        ExplicitWidth = 55
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 217
        ExplicitLeft = 219
      end
      inherited sbInsert: TSpeedButton
        Left = 257
        ExplicitLeft = 259
      end
      inherited sbDelete: TSpeedButton
        Left = 297
        ExplicitLeft = 300
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 426
      Height = 91
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowMoving, goEditing, goAlwaysShowEditor]
      OnSetEditText = frameGridFilesGridSetEditText
      OnButtonClick = frameGridFilesGridButtonClick
      Columns = <
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = True
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      ExplicitWidth = 422
      ExplicitHeight = 90
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 132
    Width = 426
    Height = 135
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 131
    ExplicitWidth = 422
    DesignSize = (
      426
      135)
    object lblModel: TLabel
      Left = 8
      Top = 65
      Width = 43
      Height = 18
      Caption = 'Model'
    end
    object btnHelp: TBitBtn
      Left = 149
      Top = 94
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnHelpClick
      ExplicitLeft = 145
    end
    object btnOK: TBitBtn
      Left = 238
      Top = 94
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnOKClick
      ExplicitLeft = 234
    end
    object btnCancel: TBitBtn
      Left = 327
      Top = 94
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 4
      ExplicitLeft = 323
    end
    object btnOpenFiles: TButton
      Left = 8
      Top = 94
      Width = 97
      Height = 33
      Caption = 'Open files'
      TabOrder = 1
      OnClick = btnOpenFilesClick
    end
    object comboModel: TComboBox
      Left = 57
      Top = 62
      Width = 353
      Height = 26
      Style = csDropDownList
      TabOrder = 0
    end
    object rgEvaluatedAt: TRadioGroup
      Left = 8
      Top = 7
      Width = 257
      Height = 49
      Caption = 'Evaluated at'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Elements'
        'Nodes')
      TabOrder = 5
    end
  end
  object dlgOpenFiles: TOpenDialog
    Filter = 
      'Text and array files|*.txt;*.arrays|Text files (*.txt)|*.txt|Arr' +
      'ay files|*.arrays|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 168
    Top = 16
  end
end
