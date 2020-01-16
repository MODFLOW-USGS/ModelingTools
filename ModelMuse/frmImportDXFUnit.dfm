inherited frmImportDXF: TfrmImportDXF
  HelpKeyword = 'Import_DXF_File_Dialog_Box'
  Caption = 'Import DXF File'
  ClientHeight = 301
  ExplicitHeight = 340
  PixelsPerInch = 96
  TextHeight = 18
  inherited rgEvaluatedAt: TRadioGroup
    Left = 9
    Top = 247
    ExplicitLeft = 9
    ExplicitTop = 247
  end
  inherited btnOK: TBitBtn
    Left = 369
    Top = 259
    OnClick = btnOKClick
    ExplicitLeft = 369
    ExplicitTop = 259
  end
  inherited btnCancel: TBitBtn
    Left = 465
    Top = 259
    ExplicitLeft = 465
    ExplicitTop = 259
  end
  inherited btnHelp: TBitBtn
    Left = 273
    Top = 259
    ExplicitLeft = 273
    ExplicitTop = 259
  end
  object cbSingleObject: TCheckBox [11]
    Left = 8
    Top = 218
    Width = 321
    Height = 17
    Caption = 'Import as single object'
    Checked = True
    State = cbChecked
    TabOrder = 9
  end
  inherited OpenDialogFile: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
  end
end
