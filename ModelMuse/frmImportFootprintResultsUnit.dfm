inherited frmImportFootprintResults: TfrmImportFootprintResults
  HelpType = htKeyword
  HelpKeyword = 'Import_Footprint_Results'
  Caption = 'Import WellFootprint Results'
  ClientHeight = 230
  ExplicitWidth = 438
  ExplicitHeight = 268
  TextHeight = 18
  object lblColorMesh: TLabel
    Left = 8
    Top = 135
    Width = 142
    Height = 18
    Caption = 'Color or contour grid'
  end
  object lblDataToImport: TLabel
    Left = 8
    Top = 8
    Width = 99
    Height = 18
    Caption = 'Data to import'
  end
  object chklstDataToImport: TCheckListBox
    Left = 8
    Top = 32
    Width = 401
    Height = 52
    ItemHeight = 18
    Items.Strings = (
      'Distributed withdrawals'
      'Footprint code')
    TabOrder = 0
    OnClickCheck = chklstDataToImportClickCheck
  end
  object rgDisplayChoice: TRadioGroup
    Left = 8
    Top = 87
    Width = 401
    Height = 42
    Caption = 'Display choice'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Color mesh'
      'Contour mesh'
      'Neither')
    TabOrder = 1
  end
  object comboColorContourGrid: TComboBox
    Left = 8
    Top = 159
    Width = 401
    Height = 26
    Style = csDropDownList
    TabOrder = 2
  end
  object btnHelp: TBitBtn
    Left = 146
    Top = 188
    Width = 82
    Height = 30
    Anchors = [akTop, akRight]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 3
    OnClick = btnHelpClick
    ExplicitLeft = 142
  end
  object btnOK: TBitBtn
    Left = 234
    Top = 188
    Width = 82
    Height = 30
    Anchors = [akTop, akRight]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 4
    OnClick = btnOKClick
    ExplicitLeft = 230
  end
  object btnCancel: TBitBtn
    Left = 322
    Top = 188
    Width = 83
    Height = 30
    Anchors = [akTop, akRight]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 5
    ExplicitLeft = 318
  end
  object dlgOpenFootprintFile: TJvOpenDialog
    DefaultExt = '.nod'
    Filter = 
      'Footprint output files (*.fpb; *.fpt)|*.fpb; *.fpt|Footprint bin' +
      'ary output files (*.fpb)|*.fpb|Footprint output files (*.fpt)|*.' +
      'fpt'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Height = 0
    Width = 0
    Left = 136
    Top = 96
  end
end
