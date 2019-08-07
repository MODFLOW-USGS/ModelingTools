inherited frmErrorsAndWarnings: TfrmErrorsAndWarnings
  HelpType = htKeyword
  HelpKeyword = 'Errors_and_Warnings_Dialog_Box'
  Caption = 'Errors and Warnings'
  ClientWidth = 472
  OnResize = FormResize
  ExplicitWidth = 490
  ExplicitHeight = 271
  PixelsPerInch = 120
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 185
    Width = 472
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      472
      41)
    object btnClose: TBitBtn
      Left = 374
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 4
    end
    object btnHelp: TBitBtn
      Left = 279
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
    end
    object btnSave: TButton
      Left = 88
      Top = 4
      Width = 75
      Height = 33
      Caption = 'Save'
      TabOrder = 1
      OnClick = btnSaveClick
    end
    object btnClear: TButton
      Left = 169
      Top = 4
      Width = 75
      Height = 33
      Caption = 'Clear'
      TabOrder = 2
      OnClick = btnClearClick
    end
    object btnCopy: TButton
      Left = 7
      Top = 4
      Width = 75
      Height = 33
      Caption = 'Copy'
      TabOrder = 0
      OnClick = btnCopyClick
    end
  end
  object vstWarningsAndErrors: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 472
    Height = 185
    Align = alClient
    CheckImageKind = ckLightTick
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag]
    PopupMenu = pmSelectEdit
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toVariableNodeHeight, toEditOnClick]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnContextPopup = vstWarningsAndErrorsContextPopup
    OnGetText = vstWarningsAndErrorsGetText
    OnInitNode = vstWarningsAndErrorsInitNode
    OnMeasureItem = vstWarningsAndErrorsMeasureItem
    Columns = <
      item
        Position = 0
        Width = 200
      end>
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 136
    Top = 72
  end
  object sdSaveFileDlg: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 176
    Top = 88
  end
  object pmSelectEdit: TPopupMenu
    Left = 256
    Top = 80
    object miSelect: TMenuItem
      Caption = 'Select'
      Hint = 'Select the related object'
      OnClick = miSelectClick
    end
    object miEdit: TMenuItem
      Caption = 'Edit...'
      Hint = 'Edit the related object in the Object Properties dialog box'
      OnClick = miEditClick
    end
    object miGoto: TMenuItem
      Caption = 'Go to'
      Hint = 'Go to the location of the related object'
      OnClick = miGotoClick
    end
  end
end
