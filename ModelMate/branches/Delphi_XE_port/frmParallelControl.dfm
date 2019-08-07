object FormParallelControl: TFormParallelControl
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'UCODE Parallel-Processing Control'
  ClientHeight = 519
  ClientWidth = 448
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object ckbxUseParallelProcessing: TCheckBox
    Left = 32
    Top = 8
    Width = 220
    Height = 17
    Caption = 'Use Parallel Processing'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object btnCancel: TBitBtn
    Left = 231
    Top = 486
    Width = 75
    Height = 25
    TabOrder = 6
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 150
    Top = 486
    Width = 75
    Height = 25
    TabOrder = 5
    OnClick = btnOKClick
    Kind = bkOK
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 318
    Width = 431
    Height = 84
    Caption = 'Copying Files to Runner Directories'
    TabOrder = 3
    object btnFilesForRunners: TSpeedButton
      Left = 16
      Top = 18
      Width = 195
      Height = 36
      Caption = 'Files for Runner Directories...'
      OnClick = btnFilesForRunnersClick
    end
    object btnPopulateRunnerDirectories: TSpeedButton
      Left = 223
      Top = 18
      Width = 195
      Height = 36
      Caption = 'Populate Runner Directories'
      OnClick = btnPopulateRunnerDirectoriesClick
    end
    object cbAutoPopRunDirs: TCheckBox
      Left = 16
      Top = 60
      Width = 203
      Height = 17
      Caption = 'Auto Populate Runner Directories'
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 412
    Width = 431
    Height = 64
    Caption = 'Starting and Stopping Runners'
    TabOrder = 4
    object ckbxAutoStart: TCheckBox
      Left = 16
      Top = 20
      Width = 161
      Height = 18
      Caption = 'AutoStart Local Runners'
      TabOrder = 0
    end
    object ckbxAutoStop: TCheckBox
      Left = 16
      Top = 40
      Width = 145
      Height = 18
      Caption = 'AutoStop Runners'
      TabOrder = 1
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 31
    Width = 431
    Height = 154
    Caption = 'Runner Directories Setup'
    TabOrder = 1
    object btnRunnerDirectories: TSpeedButton
      Left = 16
      Top = 23
      Width = 195
      Height = 36
      Caption = 'Define Runner Directories...'
      OnClick = btnRunnerDirectoriesClick
    end
    object Label5: TLabel
      Left = 95
      Top = 68
      Width = 152
      Height = 14
      Caption = 'Number of Runners Defined'
    end
    object Label6: TLabel
      Left = 95
      Top = 96
      Width = 201
      Height = 14
      Caption = 'Number of Runners with Use = True'
    end
    object Label1: TLabel
      Left = 95
      Top = 124
      Width = 254
      Height = 14
      Caption = 'Number of Runners to Use in Next Parallel Run'
    end
    object edNumRun: TEdit
      Left = 16
      Top = 65
      Width = 73
      Height = 22
      AutoSelect = False
      Color = clInfoBk
      Enabled = False
      TabOrder = 0
      Text = 'edNumRun'
    end
    object edNumUseTrue: TEdit
      Left = 16
      Top = 93
      Width = 73
      Height = 22
      Color = clInfoBk
      Enabled = False
      TabOrder = 1
      Text = 'edNumUseTrue'
    end
    object spedNumToUse: TSpinEdit
      Left = 16
      Top = 121
      Width = 73
      Height = 23
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
  end
  object GroupBox4: TGroupBox
    Left = 9
    Top = 194
    Width = 431
    Height = 114
    Caption = 'Runner Settings'
    TabOrder = 2
    object Label2: TLabel
      Left = 247
      Top = 25
      Width = 84
      Height = 14
      Caption = 'VerboseRunner'
    end
    object Label3: TLabel
      Left = 95
      Top = 53
      Width = 124
      Height = 14
      Caption = 'Wait time (in seconds)'
    end
    object Label4: TLabel
      Left = 95
      Top = 83
      Width = 84
      Height = 14
      Caption = 'Timeout Factor'
    end
    object cmbVerboseRunner: TComboBox
      Left = 16
      Top = 22
      Width = 225
      Height = 22
      ItemHeight = 14
      TabOrder = 0
      Text = 'cmbVerboseRunner'
      Items.Strings = (
        '0 - No extraneous output'
        '1 - Warnings'
        '2 - Warnings, notes'
        '3 - Warnings, notes, selected input'
        '4 - Warnings, notes, all input'
        '5 - All available output')
    end
    object veWait: TJvValidateEdit
      Left = 16
      Top = 50
      Width = 73
      Height = 22
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      DisplayFormat = dfFloatGeneral
      DecimalPlaces = 6
      EditText = '0.001'
      HasMinValue = True
      MaxValue = 100000000.000000000000000000
      MinValue = 0.000010000000000000
      TabOrder = 1
    end
    object veTimeOutFactor: TJvValidateEdit
      Left = 16
      Top = 78
      Width = 73
      Height = 22
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloatGeneral
      DecimalPlaces = 2
      EditText = '3'
      TabOrder = 2
    end
  end
end
