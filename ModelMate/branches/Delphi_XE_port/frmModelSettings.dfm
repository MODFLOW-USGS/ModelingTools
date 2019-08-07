object FormModflow: TFormModflow
  Left = 0
  Top = 0
  Caption = 'Model Settings'
  ClientHeight = 321
  ClientWidth = 666
  Color = clBtnFace
  Constraints.MaxHeight = 355
  Constraints.MinHeight = 355
  Constraints.MinWidth = 514
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    666
    321)
  PixelsPerInch = 96
  TextHeight = 16
  object lblNameFile: TLabel
    Left = 13
    Top = 103
    Width = 233
    Height = 16
    Caption = 'MODFLOW Name File for Primary Model:'
  end
  object lblExpl: TLabel
    Left = 178
    Top = 10
    Width = 114
    Height = 16
    Caption = 'Identify model type:'
  end
  object lblNameFilePred: TLabel
    Left = 13
    Top = 165
    Width = 244
    Height = 16
    Caption = 'MODFLOW Name File for Predictive Model:'
  end
  object Label1: TLabel
    Left = 30
    Top = 242
    Width = 474
    Height = 16
    Caption = 
      '(uncheck if project includes user-prepared template files or mul' +
      'tiple template files)'
  end
  object edtNameFile: TJvFilenameEdit
    Left = 13
    Top = 120
    Width = 638
    Height = 24
    Filter = 'MODFLOW Name File (*.nam)|*.nam|All files (*.*)|*.*'
    DialogOptions = [ofHideReadOnly, ofFileMustExist]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = edtNameFileChange
    ExplicitWidth = 637
  end
  object rbModflow2005: TRadioButton
    Left = 178
    Top = 31
    Width = 113
    Height = 17
    Caption = 'MODFLOW-2005'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = rbModflow2005Click
  end
  object rbModflow2000: TRadioButton
    Left = 178
    Top = 50
    Width = 272
    Height = 17
    Caption = 'MODFLOW-2000 (not currently supported)'
    Enabled = False
    TabOrder = 1
    OnClick = rbModflow2000Click
  end
  object edtNameFilePred: TJvFilenameEdit
    Left = 13
    Top = 182
    Width = 638
    Height = 24
    Filter = 'MODFLOW Name File (*.nam)|*.nam|All files (*.*)|*.*'
    DialogOptions = [ofHideReadOnly, ofFileMustExist]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnChange = edtNameFilePredChange
    ExplicitWidth = 637
  end
  object rbGeneric: TRadioButton
    Left = 178
    Top = 69
    Width = 261
    Height = 17
    Caption = 'Generic'
    TabOrder = 2
    OnClick = rbGenericClick
  end
  object cbLinkTemplate: TCheckBox
    Left = 13
    Top = 224
    Width = 637
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Link Template File with Parameters Table '
    TabOrder = 5
    WordWrap = True
    ExplicitWidth = 636
  end
  object btnCancel: TBitBtn
    Left = 348
    Top = 282
    Width = 75
    Height = 25
    TabOrder = 7
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 262
    Top = 282
    Width = 75
    Height = 25
    TabOrder = 6
    OnClick = btnOKClick
    Kind = bkOK
  end
end
