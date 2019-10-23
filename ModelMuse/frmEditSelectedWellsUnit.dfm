inherited frmEditSelectedWells: TfrmEditSelectedWells
  HelpType = htKeyword
  HelpKeyword = 'Edit_Selected_Wells_Dialog_Box'
  Caption = 'Edit Selected Wells'
  PixelsPerInch = 96
  TextHeight = 18
  inline frameWells: TframeAvailableObjects
    Left = 0
    Top = 0
    Width = 424
    Height = 185
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 424
    ExplicitHeight = 185
    inherited lblSrcObjects: TLabel
      Width = 148
      Height = 18
      Caption = 'Available well objects'
      ExplicitWidth = 148
      ExplicitHeight = 18
    end
    inherited lblDstObjects: TLabel
      Left = 216
      Width = 146
      Height = 18
      Caption = 'Selected well objects'
      ExplicitLeft = 216
      ExplicitWidth = 146
      ExplicitHeight = 18
    end
    inherited lbSrcObjects: TJvListBox
      ItemHeight = 18
    end
    inherited lbDstObjects: TJvListBox
      ItemHeight = 18
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 185
    Width = 424
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      424
      41)
    object btnHelp: TBitBtn
      Left = 143
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnCancelBtn: TBitBtn
      Left = 329
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOkBtn: TBitBtn
      Left = 236
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
    end
  end
end
