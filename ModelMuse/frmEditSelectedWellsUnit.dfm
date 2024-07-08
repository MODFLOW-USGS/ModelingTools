inherited frmEditSelectedWells: TfrmEditSelectedWells
  HelpType = htKeyword
  HelpKeyword = 'Edit_Selected_Wells_Dialog_Box'
  Caption = 'Edit Selected Wells'
  ClientHeight = 227
  ClientWidth = 386
  ExplicitWidth = 402
  ExplicitHeight = 266
  TextHeight = 18
  inline frameWells: TframeAvailableObjects
    Left = 0
    Top = 0
    Width = 386
    Height = 186
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 764
    ExplicitHeight = 160
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
    Top = 186
    Width = 386
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 160
    ExplicitWidth = 764
    DesignSize = (
      386
      41)
    object btnHelp: TBitBtn
      Left = 105
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnCancelBtn: TBitBtn
      Left = 291
      Top = 6
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOkBtn: TBitBtn
      Left = 198
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
