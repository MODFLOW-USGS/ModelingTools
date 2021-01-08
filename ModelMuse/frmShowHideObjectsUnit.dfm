inherited frmShowHideObjects: TfrmShowHideObjects
  Left = 170
  Top = 162
  HelpType = htKeyword
  HelpKeyword = 'Show_or_Hide_Objects_Dialog_Box'
  Caption = 'Show or Hide Objects'
  KeyPreview = True
  Position = poDesigned
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 18
  inherited pnlBottom: TPanel
    Top = 256
    Height = 158
    ExplicitTop = 256
    ExplicitHeight = 158
    inherited btnClose: TBitBtn
      Left = 322
      Top = 109
      TabOrder = 2
      ExplicitLeft = 322
      ExplicitTop = 109
    end
    inherited btnHelp: TBitBtn
      Left = 322
      Top = 70
      TabOrder = 1
      OnClick = btnHelpClick
      ExplicitLeft = 322
      ExplicitTop = 70
    end
    object grpShowOrSelect: TGroupBox
      Left = 1
      Top = 1
      Width = 289
      Height = 156
      Align = alLeft
      Caption = 'Show or select objects'
      TabOrder = 0
      object rgShowOrSelect: TRadioGroup
        Left = 8
        Top = 16
        Width = 177
        Height = 46
        Caption = 'Choice'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Show'
          'Select')
        TabOrder = 0
        OnClick = rgShowOrSelectClick
      end
      object rgOrientation: TRadioGroup
        Left = 191
        Top = 16
        Width = 89
        Height = 87
        Caption = 'Orientation'
        Enabled = False
        ItemIndex = 0
        Items.Strings = (
          'Top'
          'Front'
          'Side')
        TabOrder = 1
      end
      object edSearchTerm: TEdit
        Left = 8
        Top = 85
        Width = 177
        Height = 26
        TabOrder = 2
        TextHint = 'Search Term'
      end
      object btnShowOrSelect: TButton
        Left = 8
        Top = 117
        Width = 272
        Height = 25
        Caption = 'Show objects containing search term'
        TabOrder = 3
        OnClick = btnShowOrSelectClick
      end
    end
    object btnEditAllSelected: TButton
      Left = 322
      Top = 6
      Width = 89
      Height = 58
      Caption = 'Edit selected objects'
      TabOrder = 3
      WordWrap = True
      OnClick = btnEditAllSelectedClick
    end
  end
  inherited vstObjects: TVirtualStringTree
    Height = 256
    Images = ilAngles
    PopupMenu = pmSelectEdit
    OnChecked = vstObjectsChecked
    OnContextPopup = vstObjectsContextPopup
    OnDblClick = miEditClick
    OnPaintText = vstObjectsPaintText
    OnGetImageIndexEx = vstObjectsGetImageIndexEx
    OnMouseDown = vstObjectsMouseDown
    ExplicitHeight = 256
  end
  object pmSelectEdit: TPopupMenu
    Left = 144
    Top = 112
    object miSelect: TMenuItem
      Caption = 'Select'
      Enabled = False
      Hint = 'Select an object'
      OnClick = miSelectClick
    end
    object miAddToSelection: TMenuItem
      Caption = 'Add to selection'
      Enabled = False
      OnClick = miAddToSelectionClick
    end
    object miDeselect: TMenuItem
      Caption = 'Deselect'
      Enabled = False
      OnClick = miDeselectClick
    end
    object miEdit: TMenuItem
      Caption = 'Edit'
      Enabled = False
      Hint = 'Edit an object in the Object Properties dialog box'
      OnClick = miEditClick
    end
    object miGoto: TMenuItem
      Caption = 'Go to'
      Hint = 'Go to the location of the object'
      OnClick = miGotoClick
    end
  end
  object ilAngles: TImageList
    Height = 20
    Masked = False
    Width = 20
    Left = 248
    Top = 80
  end
  object ilDifferentAngle: TImageList
    Height = 20
    Masked = False
    Width = 20
    Left = 304
    Top = 80
  end
end
