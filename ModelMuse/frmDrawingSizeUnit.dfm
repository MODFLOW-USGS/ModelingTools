inherited frmDrawingSize: TfrmDrawingSize
  Left = 261
  Top = 351
  Width = 489
  Height = 348
  VertScrollBar.Range = 0
  HorzScrollBar.Range = 0
  ActiveControl = frameTop
  BorderStyle = fbsDialog
  Caption = 'Display Size'
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 120
  inline frameTop: TframeDrawingSize
    Left = 8
    Top = 8
    Width = 238
    Height = 166
    Font.Color = clBlack
    Font.Height = 18
    Font.Name = 'arial'
    Font.Pitch = fpVariable
    Font.Style = []
    Font.Weight = 48
    ParentFont = False
    TabOrder = 0
    inherited gbDrawingArea: TGroupBox
      Caption = 'Top'
      inherited adeMin1: TArgusDataEntry
        OnChange = frameTopadeMax1Change
      end
      inherited adeMax1: TArgusDataEntry
        OnChange = frameTopadeMax1Change
      end
      inherited adeMin2: TArgusDataEntry
        OnChange = frameTopadeMax2Change
      end
      inherited adeMax2: TArgusDataEntry
        OnChange = frameTopadeMax2Change
      end
    end
  end
  inline frameSide: TframeDrawingSize
    Left = 248
    Top = 8
    Width = 238
    Height = 166
    Font.Color = clBlack
    Font.Height = 18
    Font.Name = 'arial'
    Font.Pitch = fpVariable
    Font.Style = []
    Font.Weight = 48
    ParentFont = False
    TabOrder = 1
    inherited gbDrawingArea: TGroupBox
      Caption = 'Side'
      inherited adeMin1: TArgusDataEntry
        OnChange = frameSideadeMax1Change
      end
      inherited lblMin1: TLabel
        Width = 93
        Caption = 'Minimum Y:'
      end
      inherited adeMax1: TArgusDataEntry
        OnChange = frameSideadeMax1Change
      end
      inherited lblMax1: TLabel
        Width = 97
        Caption = 'Maximum Y:'
      end
      inherited adeMin2: TArgusDataEntry
        OnChange = frameSideadeMax2Change
      end
      inherited lblMin2: TLabel
        Width = 92
        Caption = 'Minimum Z:'
      end
      inherited lblMax2: TLabel
        Width = 96
        Caption = 'Maximum Z:'
      end
      inherited adeMax2: TArgusDataEntry
        OnChange = frameSideadeMax2Change
      end
    end
  end
  inline frameFront: TframeDrawingSize
    Left = 8
    Top = 176
    Width = 238
    Height = 166
    Font.Color = clBlack
    Font.Height = 18
    Font.Name = 'arial'
    Font.Pitch = fpVariable
    Font.Style = []
    Font.Weight = 48
    ParentFont = False
    TabOrder = 2
    inherited gbDrawingArea: TGroupBox
      Caption = 'Front'
      inherited adeMin1: TArgusDataEntry
        OnChange = frameFrontadeMax1Change
      end
      inherited adeMax1: TArgusDataEntry
        OnChange = frameFrontadeMax1Change
      end
      inherited adeMin2: TArgusDataEntry
        OnChange = frameFrontadeMax2Change
      end
      inherited lblMin2: TLabel
        Width = 92
        Caption = 'Minimum Z:'
      end
      inherited lblMax2: TLabel
        Width = 96
        Caption = 'Maximum Z:'
      end
      inherited adeMax2: TArgusDataEntry
        OnChange = frameFrontadeMax2Change
      end
    end
  end
  object btnCancel: TBitBtn
    Left = 384
    Top = 310
    Width = 97
    Height = 33
    ParentColor = True
    TabOrder = 4
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 280
    Top = 310
    Width = 97
    Height = 33
    ParentColor = True
    TabOrder = 3
    OnClick = btnOKClick
    Kind = bkOK
  end
end
