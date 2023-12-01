object Form1: TForm1
  Left = 222
  Top = 58
  Width = 762
  Height = 499
  Caption = 'Curve fit'
  Color = clNavy
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 754
    Height = 427
    Align = alClient
    Visible = False
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 754
    Height = 427
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    Visible = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 427
    Width = 754
    Height = 26
    Align = alBottom
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 52
      Height = 13
      Caption = 'Processing'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object Gauge1: TGauge
      Left = 72
      Top = 8
      Width = 100
      Height = 15
      ForeColor = clNavy
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Progress = 0
      Visible = False
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'dat'
    Filter = 'Data files|*.dat'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 16
    Top = 48
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 16
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open'
        ShortCut = 114
        OnClick = Open1Click
      end
      object Quit1: TMenuItem
        Caption = '&Quit'
        OnClick = Quit1Click
      end
    end
    object Compute1: TMenuItem
      Caption = '&Compute'
      object Selectmodel1: TMenuItem
        Caption = '&Select Model'
        ShortCut = 116
        OnClick = SelectModel1Click
      end
      object SelectAlgorithm1: TMenuItem
        Caption = 'Select &Algorithm'
        ShortCut = 16500
        OnClick = SelectAlgorithm1Click
      end
      object FitModel1: TMenuItem
        Caption = 'Fit &Model'
        ShortCut = 117
        OnClick = FitModel1Click
      end
      object ViewResults1: TMenuItem
        Caption = '&View Results'
        OnClick = ViewResults1Click
      end
    end
    object Graph1: TMenuItem
      Caption = '&Graph'
      object Options1: TMenuItem
        Caption = '&Options'
        ShortCut = 118
        OnClick = Options1Click
      end
      object AxesandCurves1: TMenuItem
        Caption = '&Axes and Curves'
        ShortCut = 8310
        OnClick = AxesandCurves1Click
      end
      object PlotGraph1: TMenuItem
        Caption = '&Plot Graph'
        ShortCut = 119
        OnClick = PlotGraph1Click
      end
      object PrintGraph1: TMenuItem
        Caption = 'P&rint Graph'
        OnClick = PrintGraph1Click
      end
    end
  end
end
