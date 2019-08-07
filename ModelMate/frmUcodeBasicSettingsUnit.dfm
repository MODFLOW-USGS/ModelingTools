object FormUcodeFileNames: TFormUcodeFileNames
  Left = 0
  Top = 0
  Caption = 'UCODE Input and Output File Names'
  ClientHeight = 271
  ClientWidth = 941
  Color = clBtnFace
  Constraints.MaxHeight = 360
  Constraints.MinHeight = 303
  Constraints.MinWidth = 582
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
    941
    271)
  PixelsPerInch = 120
  TextHeight = 18
  object gbMainModel: TGroupBox
    Left = 9
    Top = 9
    Width = 921
    Height = 103
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Primary Model (e.g. Forward, Sensitivity Analysis, Parameter Est' +
      'imation modes)'
    TabOrder = 0
    DesignSize = (
      921
      103)
    object lblOutputPrefix: TLabel
      Left = 160
      Top = 65
      Width = 202
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taRightJustify
      Caption = 'Output prefix:'
    end
    object lblMainInputFile: TLabel
      Left = 0
      Top = 31
      Width = 362
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taRightJustify
      Caption = 'UCODE input file for analyzing primary model:'
    end
    object edtOutputPrefix: TEdit
      Left = 376
      Top = 61
      Width = 538
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'edtOutputPrefixMain'
    end
    object fedMainInputFileName: TJvFilenameEdit
      Left = 376
      Top = 27
      Width = 538
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DefaultExt = '.in'
      Filter = 'UCODE in files (*.in)|*.in|All files (*.*)|*.*'
      ButtonWidth = 25
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'fedMainInputFileName'
    end
  end
  object gbPredModel: TGroupBox
    Left = 10
    Top = 120
    Width = 921
    Height = 97
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Predictive Model'
    TabOrder = 1
    DesignSize = (
      921
      97)
    object lblPredInputName: TLabel
      Left = 7
      Top = 24
      Width = 355
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taRightJustify
      Caption = 'UCODE input file for analyzing predictive model:'
    end
    object Label1: TLabel
      Left = 166
      Top = 58
      Width = 195
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taRightJustify
      Caption = 'Output prefix:'
    end
    object edtOutputPrefixPred: TEdit
      Left = 376
      Top = 54
      Width = 541
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'edtOutputPrefixPred'
    end
    object fedPredInputFileName: TJvFilenameEdit
      Left = 376
      Top = 20
      Width = 538
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DefaultExt = '.in'
      Filter = 'UCODE in files (*.in)|*.in|All files (*.*)|*.*'
      ButtonWidth = 25
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'fedPredInputFileName'
    end
  end
  object btnCancel: TBitBtn
    Left = 476
    Top = 224
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
    Left = 380
    Top = 224
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
