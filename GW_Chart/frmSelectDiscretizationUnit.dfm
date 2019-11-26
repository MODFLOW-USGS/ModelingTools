object frmSelectDiscretization: TfrmSelectDiscretization
  Left = 324
  Top = 163
  Width = 483
  Height = 312
  Caption = 'Select Discretization File'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Times New Roman'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 19
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 425
    Height = 57
    Caption = 
      'If this model is a MODFLOW-2000 or MODFLOW-2005 model, GW_Chart ' +
      'can calculate the cumulative volumes for each time step by multi' +
      'plying by the time step lengths.  '
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 289
    Height = 19
    Caption = 'To do this, you must select the discretization file.'
  end
  object Label3: TLabel
    Left = 8
    Top = 72
    Width = 400
    Height = 19
    Caption = 
      'All stress periods and time steps must be included for this to w' +
      'ork.'
  end
  object rgChoose: TRadioGroup
    Left = 9
    Top = 120
    Width = 448
    Height = 49
    Caption = 'Read Discretization file?'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Yes'
      'No')
    TabOrder = 0
    OnClick = rgChooseClick
  end
  inline framDisFile: TframFilePath
    Left = 8
    Top = 176
    Width = 449
    Height = 49
    TabOrder = 1
    inherited lblFileType: TLabel
      Width = 102
      Height = 19
      Caption = 'Discretization file'
    end
    inherited edFilePath: TEdit
      Width = 365
      Height = 27
      OnChange = framDisFileedFilePathChange
      OnExit = framDisFileedFilePathExit
    end
    inherited btnBrowse: TButton
      Left = 372
    end
    inherited OpenDialogPath: TOpenDialog
      Filter = 'Discretization Files (*.dis)|*.dis|All Files (*.*)|*.*'
    end
  end
  object btnOK: TBitBtn
    Left = 380
    Top = 232
    Width = 75
    Height = 25
    Enabled = False
    TabOrder = 3
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 300
    Top = 232
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
