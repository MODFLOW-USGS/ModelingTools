inherited frmWetting: TfrmWetting
  Caption = 'Wetting'
  ClientHeight = 204
  ClientWidth = 464
  ExplicitWidth = 472
  ExplicitHeight = 231
  PixelsPerInch = 96
  TextHeight = 17
  object Panel4: TPanel
    Left = 0
    Top = 160
    Width = 464
    Height = 44
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      464
      44)
    object btnHelp: TBitBtn
      Left = 123
      Top = 5
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 237
      Top = 5
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 351
      Top = 5
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkCancel
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 464
    Height = 160
    Align = alClient
    TabOrder = 1
    ExplicitTop = 166
    ExplicitWidth = 533
    ExplicitHeight = 202
    object lblWettingEquation: TLabel
      Left = 8
      Top = 116
      Width = 181
      Height = 17
      Caption = 'Equation for Rewetting Cells:'
      Enabled = False
    end
    object lblWetFact: TLabel
      Left = 8
      Top = 31
      Width = 96
      Height = 17
      Caption = ' Wetting Factor'
      Enabled = False
    end
    object lblCheckDry: TLabel
      Left = 8
      Top = 75
      Width = 220
      Height = 17
      Caption = 'Iterations to  check for wetting cells'
      Enabled = False
    end
    object cbWetting: TJvCheckBox
      Left = 8
      Top = 8
      Width = 110
      Height = 17
      Caption = 'Wetting active'
      TabOrder = 0
      LinkedControls = <
        item
          Control = lblWetFact
        end
        item
          Control = adeWettingFact
        end
        item
          Control = lblCheckDry
        end
        item
          Control = seCheckDry
        end
        item
          Control = lblWettingEquation
        end
        item
          Control = comboWettingEquation
        end>
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = 17
      HotTrackFont.Name = 'Microsoft Sans Serif'
      HotTrackFont.Pitch = fpVariable
      HotTrackFont.Style = []
    end
    object comboWettingEquation: TJvImageComboBox
      Left = 195
      Top = 114
      Width = 257
      Height = 27
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      Color = clBtnFace
      DroppedWidth = 257
      Enabled = False
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 21
      ItemIndex = 0
      TabOrder = 1
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'h = BOT + WETFCT (hh-BOT) (0)'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'h = BOT + WETFCT (WETDRY) (1)'
        end>
    end
    object seCheckDry: TJvSpinEdit
      Left = 248
      Top = 75
      Width = 204
      Height = 25
      ButtonKind = bkClassic
      Enabled = False
      TabOrder = 2
    end
    object adeWettingFact: TRbwDataEntry
      Left = 144
      Top = 31
      Width = 308
      Height = 22
      Hint = 'This affects model stability'
      HelpContext = 290
      Color = clBtnFace
      Enabled = False
      ItemHeight = 17
      TabOrder = 3
      Text = '0.5'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
end
