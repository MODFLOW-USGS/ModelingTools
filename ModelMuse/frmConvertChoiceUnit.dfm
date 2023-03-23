inherited frmConvertChoice: TfrmConvertChoice
  Left = 565
  Top = 340
  HelpType = htKeyword
  HelpKeyword = 'Data_Type_Problem_Dialog_Box'
  ActiveControl = rgChoice
  Caption = 'Data Type Problem'
  ClientHeight = 235
  Position = poDesigned
  ExplicitHeight = 273
  PixelsPerInch = 120
  TextHeight = 18
  object rgChoice: TRadioGroup
    Left = 0
    Top = 113
    Width = 426
    Height = 81
    Align = alClient
    Caption = 'What do you want to do?'
    ItemIndex = 0
    Items.Strings = (
      'Change the data type of the data set.'
      'Automatically adjust the formula')
    TabOrder = 0
    ExplicitWidth = 359
    ExplicitHeight = 69
  end
  object pnlButton: TPanel
    Left = 0
    Top = 194
    Width = 426
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 182
    ExplicitWidth = 359
    object btnCancel: TBitBtn
      Left = 264
      Top = 4
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 168
      Top = 4
      Width = 91
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
    end
    object btnHelp: TBitBtn
      Left = 71
      Top = 6
      Width = 91
      Height = 33
      HelpType = htKeyword
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 113
    Align = alTop
    ParentColor = True
    TabOrder = 2
    ExplicitWidth = 359
    object Label1: TLabel
      Left = 16
      Top = 8
      Width = 275
      Height = 18
      Caption = 'Error: The result data type of the formula'
    end
    object lblFormulaDataType: TLabel
      Left = 16
      Top = 56
      Width = 42
      Height = 18
      Caption = '(Real)'
    end
    object lblDataSetDataType: TLabel
      Left = 184
      Top = 80
      Width = 61
      Height = 18
      Caption = '(Integer).'
    end
    object Label3: TLabel
      Left = 16
      Top = 80
      Width = 139
      Height = 18
      Caption = 'type of the data set: '
    end
    object Label2: TLabel
      Left = 112
      Top = 56
      Width = 209
      Height = 18
      Caption = 'is not compatible with the data'
    end
    object Label4: TLabel
      Left = 16
      Top = 32
      Width = 103
      Height = 18
      Caption = 'for the data set'
    end
    object lblVariableName: TLabel
      Left = 144
      Top = 32
      Width = 110
      Height = 18
      Caption = '(VariableName)'
    end
  end
end
