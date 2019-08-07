object frmUcodeSettings: TfrmUcodeSettings
  Left = 0
  Top = 0
  Caption = 'UCODE_2005 Settings'
  ClientHeight = 667
  ClientWidth = 882
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 584
    Width = 411
    Height = 39
    Caption = 
      'When complete, this window will define Name, PathToMergedFile, S' +
      'oSFile, StdErrOne, EigenValues, StartRes, IntermedRes, FinalRes,' +
      ' StartSens, IntermedSens, FinalSens, DataExchange, and CreateIni' +
      'tFiles'
    WordWrap = True
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 648
    Width = 882
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Text = 'Status'
        Width = 50
      end
      item
        Width = 50
      end>
  end
end
