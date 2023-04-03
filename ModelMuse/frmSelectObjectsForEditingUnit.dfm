inherited frmSelectObjectsForEditing: TfrmSelectObjectsForEditing
  HelpType = htKeyword
  HelpKeyword = 'Select_Objects_for_Editing'
  Caption = 'Select Objects for Editing or Deletion'
  ClientWidth = 547
  ExplicitWidth = 559
  ExplicitHeight = 449
  PixelsPerInch = 120
  TextHeight = 18
  inherited pnlBottom: TPanel
    Top = 325
    Width = 547
    Height = 86
    TabOrder = 2
    ExplicitTop = 324
    ExplicitWidth = 543
    ExplicitHeight = 86
    inherited btnClose: TBitBtn
      Left = 445
      Top = 46
      Kind = bkCancel
      TabOrder = 6
      ExplicitLeft = 441
      ExplicitTop = 46
    end
    inherited btnHelp: TBitBtn
      Left = -2
      Top = 46
      TabOrder = 2
      OnClick = btnHelpClick
      ExplicitLeft = -6
      ExplicitTop = 46
    end
    object btnOK: TBitBtn
      Left = 188
      Top = 46
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Caption = '&Edit'
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnOKClick
      ExplicitLeft = 184
    end
    object btnDelete: TBitBtn
      Left = 93
      Top = 46
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      Glyph.Data = {
        42020000424D40020000000000008E0000002800000018000000180000000100
        08000100000040020000120B0000120B000016000000000000000732DE000632
        E1000533E9000434F5000335F900153EE100143FEC001341F7001440F000244C
        EE003558E6003459EE00335AF200325BF9004565E6005472E900637FF0006180
        F900738CF000FF00FF008298F1008099FC001713011300001513010501000113
        0000141302010105011300000004130F00000F13020101050113011300000003
        130E000002000C13000412010105021301130000021303000003011313000813
        0004100102050313011300000313000405000101081300041002020504130113
        0000041300040A010101061300040B020106051301130000051300040F010202
        041300040B0202060613011300000613000414020202021300040B0202060713
        0113000008130003060202000302000306131300061301130000091300050902
        0303060009130113000009130005080302030C00091301130000081300040302
        0302020300030C131300061301130000071302030004020C1313020300030D13
        130005130113000005130005110303020C000413000307040D00061301130000
        04130005110303030C00061300030D040D000513011300000313000511030303
        0C000813000311040D00041301130000021300050D0404030C000A1300031504
        07000313011300000006130D0404030D0D130003071113000113011300000005
        130404040D001213011300000004130D040D1313011300001713011300001713
        011300000001}
      ModalResult = 1
      TabOrder = 3
      OnClick = btnDeleteClick
      ExplicitLeft = 89
    end
    object btnEditFeature: TButton
      Left = 283
      Top = 46
      Width = 156
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'Edit feature formula'
      ModalResult = 8
      TabOrder = 5
      OnClick = btnEditFeatureClick
      ExplicitLeft = 279
    end
    object btnCheckSelected: TButton
      Left = 2
      Top = 6
      Width = 184
      Height = 34
      Action = acCheckSelected
      TabOrder = 0
    end
    object btnCheckSelected1: TButton
      Left = 192
      Top = 6
      Width = 184
      Height = 34
      Action = acUncheckSelected
      TabOrder = 1
    end
  end
  inherited vstObjects: TVirtualStringTree
    Top = 57
    Width = 547
    Height = 268
    PopupMenu = pmChangeStates
    TabOrder = 1
    TreeOptions.SelectionOptions = [toMultiSelect]
    OnChecked = vstObjectsChecked
    ExplicitTop = 57
    ExplicitWidth = 547
    ExplicitHeight = 268
  end
  object rgViewDirection: TRadioGroup
    Left = 0
    Top = 0
    Width = 547
    Height = 57
    Align = alTop
    Caption = 'View Direction'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Top'
      'Front'
      'Side')
    TabOrder = 0
    OnClick = rgViewDirectionClick
    ExplicitWidth = 543
  end
  object pmChangeStates: TPopupMenu
    Left = 304
    Top = 88
    object miCheckSelected: TMenuItem
      Action = acCheckSelected
    end
    object miUncheckSelected: TMenuItem
      Action = acUncheckSelected
    end
  end
  object actlst1: TActionList
    Left = 392
    Top = 152
    object acCheckSelected: TAction
      Caption = 'Check selected'
      OnExecute = acCheckSelectedClick
    end
    object acUncheckSelected: TAction
      Caption = 'Uncheck selected'
      OnExecute = acUncheckSelectedClick
    end
  end
end
