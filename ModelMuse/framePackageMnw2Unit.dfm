inherited framePackageMnw2: TframePackageMnw2
  Width = 376
  Height = 318
  ExplicitWidth = 376
  ExplicitHeight = 318
  DesignSize = (
    376
    318)
  object lblPrintOption: TLabel [2]
    Left = 16
    Top = 157
    Width = 117
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Print option (MNWPRNT)'
    Enabled = False
  end
  inherited memoComments: TMemo
    Width = 345
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitWidth = 345
  end
  object comboPrintOption: TJvImageComboBox [4]
    Left = 16
    Top = 180
    Width = 145
    Height = 23
    Style = csOwnerDrawVariable
    Anchors = [akLeft, akBottom]
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 145
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 17
    ItemIndex = 2
    TabOrder = 1
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Minimum'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Intermediate'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Maximum'
      end>
  end
  object gbMnwiOptions: TGroupBox [5]
    Left = 16
    Top = 216
    Width = 345
    Height = 99
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Multi-Node Well Information (MNWI) Package Options'
    TabOrder = 2
    object cbWellOutput: TCheckBox
      Left = 3
      Top = 24
      Width = 294
      Height = 17
      Margins.Top = 8
      Caption = 'Create equivalent WEL package input file (WEL1flag)'
      TabOrder = 0
    end
    object cbSummarizeByWell: TCheckBox
      Left = 3
      Top = 47
      Width = 339
      Height = 17
      Caption = 
        'Summarize flow rates by well in a separate output file (QSUMflag' +
        ')'
      TabOrder = 1
    end
    object cbSummarizeByNode: TCheckBox
      Left = 3
      Top = 70
      Width = 339
      Height = 17
      Caption = 
        'Summarize flow rates by node in a separate output file (BYNDflag' +
        ')'
      TabOrder = 2
    end
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = lblPrintOption
      end
      item
        Control = comboPrintOption
      end>
  end
end
