object FormParallelControl: TFormParallelControl
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'UCODE Parallel-Processing Control'
  ClientHeight = 667
  ClientWidth = 576
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 19
  object ckbxUseParallelProcessing: TCheckBox
    Left = 31
    Top = 8
    Width = 283
    Height = 22
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Use Parallel Processing'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object btnCancel: TBitBtn
    Left = 297
    Top = 633
    Width = 96
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 6
  end
  object btnOK: TBitBtn
    Left = 193
    Top = 633
    Width = 96
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 5
    OnClick = btnOKClick
  end
  object GroupBox1: TGroupBox
    Left = 10
    Top = 433
    Width = 554
    Height = 108
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Copying Files to Runner Directories'
    TabOrder = 3
    object btnFilesForRunners: TSpeedButton
      Left = 21
      Top = 23
      Width = 250
      Height = 46
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Files for Runner Directories...'
      OnClick = btnFilesForRunnersClick
    end
    object btnPopulateRunnerDirectories: TSpeedButton
      Left = 287
      Top = 23
      Width = 250
      Height = 46
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Populate Runner Directories'
      OnClick = btnPopulateRunnerDirectoriesClick
    end
    object cbAutoPopRunDirs: TCheckBox
      Left = 21
      Top = 77
      Width = 516
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Auto Populate Runner Directories'
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 10
    Top = 547
    Width = 554
    Height = 82
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Starting and Stopping Runners'
    TabOrder = 4
    object ckbxAutoStart: TCheckBox
      Left = 21
      Top = 26
      Width = 207
      Height = 23
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'AutoStart Local Runners'
      TabOrder = 0
    end
    object ckbxAutoStop: TCheckBox
      Left = 21
      Top = 51
      Width = 186
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'AutoStop Runners'
      TabOrder = 1
    end
  end
  object GroupBox3: TGroupBox
    Left = 10
    Top = 37
    Width = 554
    Height = 198
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Runner Directories Setup'
    TabOrder = 1
    object btnRunnerDirectories: TSpeedButton
      Left = 21
      Top = 30
      Width = 250
      Height = 46
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Define Runner Directories...'
      OnClick = btnRunnerDirectoriesClick
    end
    object Label5: TLabel
      Left = 122
      Top = 87
      Width = 204
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Number of Runners Defined'
    end
    object Label6: TLabel
      Left = 122
      Top = 123
      Width = 260
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Number of Runners with Use = True'
    end
    object Label1: TLabel
      Left = 122
      Top = 159
      Width = 339
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Number of Runners to Use in Next Parallel Run'
    end
    object edNumRun: TEdit
      Left = 21
      Top = 84
      Width = 93
      Height = 27
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSelect = False
      Color = clInfoBk
      Enabled = False
      TabOrder = 0
      Text = 'edNumRun'
    end
    object edNumUseTrue: TEdit
      Left = 21
      Top = 120
      Width = 93
      Height = 27
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Color = clInfoBk
      Enabled = False
      TabOrder = 1
      Text = 'edNumUseTrue'
    end
    object spedNumToUse: TSpinEdit
      Left = 21
      Top = 156
      Width = 93
      Height = 29
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
  end
  object GroupBox4: TGroupBox
    Left = 10
    Top = 246
    Width = 554
    Height = 172
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Runner Settings'
    TabOrder = 2
    object Label2: TLabel
      Left = 318
      Top = 32
      Width = 113
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'VerboseRunner'
    end
    object Label3: TLabel
      Left = 122
      Top = 68
      Width = 164
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Wait time (in seconds)'
    end
    object Label4: TLabel
      Left = 122
      Top = 101
      Width = 108
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Timeout Factor'
    end
    object Label7: TLabel
      Left = 122
      Top = 136
      Width = 190
      Height = 19
      Caption = 'WaitRunners (in seconds)'
    end
    object cmbVerboseRunner: TComboBox
      Left = 21
      Top = 28
      Width = 289
      Height = 27
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
      Left = 21
      Top = 63
      Width = 93
      Height = 27
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
      Left = 21
      Top = 98
      Width = 93
      Height = 27
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloatGeneral
      DecimalPlaces = 2
      EditText = '3'
      TabOrder = 2
    end
    object veWaitRunners: TJvValidateEdit
      Left = 21
      Top = 132
      Width = 93
      Height = 27
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      DisplayFormat = dfFloatGeneral
      EditText = '0.001'
      TabOrder = 3
    end
  end
end
