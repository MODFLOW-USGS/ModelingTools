object FormParallelJupiter: TFormParallelJupiter
  Left = 0
  Top = 0
  Caption = 'Parallel Processing (JUPITER API)'
  ClientHeight = 516
  ClientWidth = 742
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblWait: TLabel
    Left = 246
    Top = 49
    Width = 237
    Height = 14
    Caption = 'WAIT: Time delay used in file management'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblVerbosity: TLabel
    Left = 246
    Top = 77
    Width = 258
    Height = 14
    Caption = 'VERBOSERUNNER: Output verbosity of runners'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblTimeOutFactor: TLabel
    Left = 246
    Top = 105
    Width = 421
    Height = 14
    Caption = 
      'TIMEOUTFACTOR: Multiplies RunTime to determine if a model run is' +
      ' overdue'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cbUseParallel: TCheckBox
    Left = 8
    Top = 11
    Width = 179
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
  object edtWait: TEdit
    Left = 8
    Top = 46
    Width = 232
    Height = 22
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = '0.001'
  end
  object cbVerbosity: TComboBox
    Left = 8
    Top = 74
    Width = 232
    Height = 22
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 14
    ItemIndex = 3
    ParentFont = False
    TabOrder = 2
    Text = '3 - Warnings, notes, selected input'
    Items.Strings = (
      '0 - No extraneous output'
      '1 - Warnings'
      '2 - Warnings, notes'
      '3 - Warnings, notes, selected input'
      '4 - Warnings, notes, all input'
      '5 - All available output')
  end
  object cbAutoStopRunners: TCheckBox
    Left = 8
    Top = 130
    Width = 659
    Height = 17
    Caption = 
      'AUTOSTOPRUNNERS: Stop Runners After Execution (Uncheck box to ha' +
      've runners reset after execution)'
    Checked = True
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    State = cbChecked
    TabOrder = 3
  end
  object edtTimeOutFactor: TEdit
    Left = 8
    Top = 102
    Width = 232
    Height = 22
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    Text = '3.0'
  end
  object DataGrid1: TDataGrid
    Left = 0
    Top = 157
    Width = 742
    Height = 359
    Align = alBottom
    ColCount = 3
    FixedCols = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    ParentFont = False
    TabOrder = 5
    Columns = <
      item
        Title.Caption = 'Runner Name'
        Title.WordWrap = False
      end
      item
        Title.Caption = 'Expected Run Time (seconds)'
        Title.WordWrap = False
      end
      item
        Title.Caption = 'Runner Directory'
        Title.WordWrap = False
      end>
    RowCountMin = 0
    SelectedIndex = 0
    Version = '2.0'
    ColWidths = (
      116
      177
      485)
  end
end
