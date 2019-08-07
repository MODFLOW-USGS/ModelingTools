object FormModflow: TFormModflow
  Left = 0
  Top = 0
  Caption = 'Model Settings'
  ClientHeight = 390
  ClientWidth = 791
  Color = clBtnFace
  Constraints.MaxHeight = 501
  Constraints.MinHeight = 422
  Constraints.MinWidth = 610
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    791
    390)
  PixelsPerInch = 120
  TextHeight = 18
  object lblNameFile: TLabel
    Left = 15
    Top = 122
    Width = 290
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'MODFLOW Name File for Primary Model:'
  end
  object lblExpl: TLabel
    Left = 211
    Top = 12
    Width = 132
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Identify model type:'
  end
  object lblNameFilePred: TLabel
    Left = 15
    Top = 196
    Width = 306
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'MODFLOW Name File for Predictive Model:'
  end
  object Label1: TLabel
    Left = 36
    Top = 287
    Width = 561
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      '(uncheck if project includes user-prepared template files or mul' +
      'tiple template files)'
  end
  object edtNameFile: TJvFilenameEdit
    Left = 15
    Top = 143
    Width = 758
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Filter = 'MODFLOW Name File (*.nam)|*.nam|All files (*.*)|*.*'
    DialogOptions = [ofHideReadOnly, ofFileMustExist]
    ButtonWidth = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = edtNameFileChange
  end
  object rbModflow2005: TRadioButton
    Left = 211
    Top = 40
    Width = 323
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'MODFLOW-2005'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = rbModflow2005Click
  end
  object rbModflow2000: TRadioButton
    Left = 211
    Top = 62
    Width = 422
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'MODFLOW-2000 (not currently supported)'
    Enabled = False
    TabOrder = 1
    OnClick = rbModflow2000Click
  end
  object edtNameFilePred: TJvFilenameEdit
    Left = 15
    Top = 216
    Width = 758
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Filter = 'MODFLOW Name File (*.nam)|*.nam|All files (*.*)|*.*'
    DialogOptions = [ofHideReadOnly, ofFileMustExist]
    ButtonWidth = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnChange = edtNameFilePredChange
  end
  object rbGeneric: TRadioButton
    Left = 211
    Top = 84
    Width = 310
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Generic'
    TabOrder = 2
    OnClick = rbGenericClick
  end
  object cbLinkTemplate: TCheckBox
    Left = 15
    Top = 266
    Width = 757
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Link Template File with Parameters Table '
    TabOrder = 5
    WordWrap = True
  end
  object btnCancel: TBitBtn
    Left = 413
    Top = 335
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 7
  end
  object btnOK: TBitBtn
    Left = 311
    Top = 335
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 6
    OnClick = btnOKClick
  end
end
