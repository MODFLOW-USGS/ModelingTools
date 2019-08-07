object FormModelDir: TFormModelDir
  Left = 0
  Top = 0
  Caption = 'Model Directories'
  ClientHeight = 163
  ClientWidth = 693
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    693
    163)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 11
    Width = 522
    Height = 16
    Caption = 
      'Primary model directory: Directory where main input file for pri' +
      'mary process model resides'
  end
  object lblPredModelDir: TLabel
    Left = 10
    Top = 72
    Width = 544
    Height = 16
    Caption = 
      'Predictive model directory: Directory where main input file for ' +
      'predictive process model resides'
  end
  object dedModelDir: TJvDirectoryEdit
    Left = 10
    Top = 31
    Width = 672
    Height = 24
    DialogKind = dkWin32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'dedModelDir'
    OnChange = dedModelDirChange
  end
  object dedPredModelDir: TJvDirectoryEdit
    Left = 10
    Top = 92
    Width = 672
    Height = 24
    DialogKind = dkWin32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'dedPredModelDir'
    OnChange = dedPredModelDirChange
  end
  object btnCancel: TBitBtn
    Left = 345
    Top = 128
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 264
    Top = 128
    Width = 75
    Height = 25
    TabOrder = 2
    OnClick = btnOKClick
    Kind = bkOK
  end
end
