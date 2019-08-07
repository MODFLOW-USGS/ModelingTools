object FormParallelJupiter: TFormParallelJupiter
  Left = 0
  Top = 0
  Caption = 'Parallel Processing (JUPITER API)'
  ClientHeight = 675
  ClientWidth = 970
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 19
  object lblWait: TLabel
    Left = 348
    Top = 64
    Width = 309
    Height = 19
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'WAIT: Time delay used in file management'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblVerbosity: TLabel
    Left = 348
    Top = 101
    Width = 351
    Height = 19
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'VERBOSERUNNER: Output verbosity of runners'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblTimeOutFactor: TLabel
    Left = 348
    Top = 137
    Width = 558
    Height = 19
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'TIMEOUTFACTOR: Multiplies RunTime to determine if a model run is' +
      ' overdue'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object cbUseParallel: TCheckBox
    Left = 10
    Top = 14
    Width = 235
    Height = 23
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
  object edtWait: TEdit
    Left = 10
    Top = 60
    Width = 330
    Height = 27
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = '0.001'
  end
  object cbVerbosity: TComboBox
    Left = 10
    Top = 97
    Width = 330
    Height = 27
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = []
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
    Left = 9
    Top = 174
    Width = 862
    Height = 22
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'AUTOSTOPRUNNERS: Stop Runners After Execution (Uncheck box to ha' +
      've runners reset after execution)'
    Checked = True
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    State = cbChecked
    TabOrder = 3
  end
  object edtTimeOutFactor: TEdit
    Left = 10
    Top = 133
    Width = 330
    Height = 27
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    Text = '3.0'
  end
  object DataGrid1: TEcDataGrid
    Left = 0
    Top = 205
    Width = 970
    Height = 470
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    ColCount = 3
    DefaultRowHeight = 20
    FixedCols = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Arial'
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
      234
      485)
  end
end
