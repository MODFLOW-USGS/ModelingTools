inherited frmErrorsAndWarnings: TfrmErrorsAndWarnings
  Left = 0
  Top = 0
  HelpType = htKeyword
  HelpKeyword = 'Errors_and_Warnings_Dialog_Box'
  Caption = 'Errors and Warnings'
  ClientHeight = 235
  ClientWidth = 472
  Position = poDefaultPosOnly
  OnResize = FormResize
  ExplicitWidth = 484
  ExplicitHeight = 273
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 194
    Width = 472
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 193
    ExplicitWidth = 468
    DesignSize = (
      472
      41)
    object btnClose: TBitBtn
      Left = 334
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 4
      ExplicitLeft = 330
    end
    object btnHelp: TBitBtn
      Left = 239
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
      ExplicitLeft = 235
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
    Height = 194
    Align = alClient
    Colors.BorderColor = 15987699
    Colors.DisabledColor = clGray
    Colors.DropMarkColor = 15385233
    Colors.DropTargetColor = 15385233
    Colors.DropTargetBorderColor = 15385233
    Colors.FocusedSelectionColor = 15385233
    Colors.FocusedSelectionBorderColor = 15385233
    Colors.GridLineColor = 15987699
    Colors.HeaderHotColor = clBlack
    Colors.HotColor = clBlack
    Colors.SelectionRectangleBlendColor = 15385233
    Colors.SelectionRectangleBorderColor = 15385233
    Colors.SelectionTextColor = clBlack
    Colors.TreeLineColor = 9471874
    Colors.UnfocusedColor = clGray
    Colors.UnfocusedSelectionColor = 13421772
    Colors.UnfocusedSelectionBorderColor = 13421772
    Header.AutoSizeIndex = -1
    Header.Height = 22
    Header.Options = [hoColumnResize, hoDrag]
    PopupMenu = pmSelectEdit
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toVariableNodeHeight, toEditOnClick]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnContextPopup = vstWarningsAndErrorsContextPopup
    OnGetText = vstWarningsAndErrorsGetText
    OnInitNode = vstWarningsAndErrorsInitNode
    OnMeasureItem = vstWarningsAndErrorsMeasureItem
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
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
    object miIgnorethistypeoferrororwarning1: TMenuItem
      Caption = 'Ignore this'
      OnClick = miIgnorethistypeoferrororwarning1Click
    end
  end
end
