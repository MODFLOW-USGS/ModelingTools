object FormUcodeFileNames: TFormUcodeFileNames
  Left = 0
  Top = 0
  Caption = 'UCODE Input and Output File Names'
  ClientHeight = 221
  ClientWidth = 792
  Color = clBtnFace
  Constraints.MaxHeight = 259
  Constraints.MinHeight = 255
  Constraints.MinWidth = 490
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    792
    221)
  PixelsPerInch = 96
  TextHeight = 16
  object gbMainModel: TGroupBox
    Left = 8
    Top = 8
    Width = 776
    Height = 87
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Primary Model (e.g. Forward, Sensitivity Analysis, Parameter Est' +
      'imation modes)'
    TabOrder = 0
    DesignSize = (
      776
      87)
    object lblOutputPrefix: TLabel
      Left = 200
      Top = 54
      Width = 79
      Height = 16
      Caption = 'Output prefix:'
    end
    object lblMainInputFile: TLabel
      Left = 17
      Top = 26
      Width = 262
      Height = 16
      Caption = 'UCODE input file for analyzing primary model:'
    end
    object edtOutputPrefix: TEdit
      Left = 284
      Top = 51
      Width = 486
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'edtOutputPrefixMain'
    end
    object fedMainInputFileName: TJvFilenameEdit
      Left = 284
      Top = 23
      Width = 486
      Height = 24
      DefaultExt = '.in'
      Filter = 'UCODE in files (*.in)|*.in|All files (*.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'fedMainInputFileName'
    end
  end
  object gbPredModel: TGroupBox
    Left = 8
    Top = 101
    Width = 776
    Height = 82
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Predictive Model'
    TabOrder = 1
    DesignSize = (
      776
      82)
    object lblPredInputName: TLabel
      Left = 6
      Top = 21
      Width = 273
      Height = 16
      Caption = 'UCODE input file for analyzing predictive model:'
    end
    object Label1: TLabel
      Left = 200
      Top = 50
      Width = 79
      Height = 16
      Caption = 'Output prefix:'
    end
    object edtOutputPrefixPred: TEdit
      Left = 284
      Top = 47
      Width = 486
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'edtOutputPrefixPred'
    end
    object fedPredInputFileName: TJvFilenameEdit
      Left = 284
      Top = 17
      Width = 486
      Height = 24
      DefaultExt = '.in'
      Filter = 'UCODE in files (*.in)|*.in|All files (*.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'fedPredInputFileName'
    end
  end
  object btnCancel: TBitBtn
    Left = 401
    Top = 189
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 320
    Top = 189
    Width = 75
    Height = 25
    TabOrder = 2
    OnClick = btnOKClick
    Kind = bkOK
  end
end
