inherited frmEditSelectedWells: TfrmEditSelectedWells
  HelpType = htKeyword
  HelpKeyword = 'Edit_Selected_Wells_Dialog_Box'
  Caption = 'Edit Selected Wells'
  ExplicitWidth = 438
  ExplicitHeight = 272
  TextHeight = 18
  inline frameWells: TframeAvailableObjects
    Left = 0
    Top = 0
    Width = 426
    Height = 193
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 422
    ExplicitHeight = 192
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
    Top = 193
    Width = 426
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 192
    ExplicitWidth = 422
    DesignSize = (
      426
      41)
    object btnHelp: TBitBtn
      Left = 139
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 135
    end
    object btnCancelBtn: TBitBtn
      Left = 325
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 321
    end
    object btnOkBtn: TBitBtn
      Left = 232
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 228
    end
  end
end
