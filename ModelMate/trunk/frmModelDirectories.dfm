object FormModelDir: TFormModelDir
  Left = 0
  Top = 0
  Caption = 'Model Directories'
  ClientHeight = 201
  ClientWidth = 822
  Color = clBtnFace
  Constraints.MaxHeight = 277
  Constraints.MinHeight = 233
  Constraints.MinWidth = 670
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
    822
    201)
  PixelsPerInch = 120
  TextHeight = 18
  object Label1: TLabel
    Left = 12
    Top = 13
    Width = 622
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'Primary model directory: Directory where main input file for pri' +
      'mary process model resides'
  end
  object lblPredModelDir: TLabel
    Left = 12
    Top = 86
    Width = 654
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'Predictive model directory: Directory where main input file for ' +
      'predictive process model resides'
  end
  object dedModelDir: TJvDirectoryEdit
    Left = 12
    Top = 37
    Width = 797
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DialogKind = dkWin32
    ButtonWidth = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'dedModelDir'
    OnChange = dedModelDirChange
  end
  object dedPredModelDir: TJvDirectoryEdit
    Left = 12
    Top = 109
    Width = 797
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DialogKind = dkWin32
    ButtonWidth = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'dedPredModelDir'
    OnChange = dedPredModelDirChange
  end
  object btnCancel: TBitBtn
    Left = 410
    Top = 152
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
    TabOrder = 3
  end
  object btnOK: TBitBtn
    Left = 314
    Top = 152
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
    TabOrder = 2
    OnClick = btnOKClick
  end
end
