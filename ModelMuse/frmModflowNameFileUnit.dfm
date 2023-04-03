inherited frmModflowNameFile: TfrmModflowNameFile
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Name_File_Dialog_Box'
  Caption = 'MODFLOW Name File'
  ClientHeight = 346
  ClientWidth = 520
  ExplicitWidth = 532
  ExplicitHeight = 384
  TextHeight = 18
  object pnlMain: TPanel
    Left = 0
    Top = 41
    Width = 520
    Height = 305
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 516
    ExplicitHeight = 304
    DesignSize = (
      520
      305)
    object lblLines: TLabel
      Left = 8
      Top = 8
      Width = 466
      Height = 54
      Caption = 
        'Additional lines to add to the MODFLOW name file.'#13#10'(Use this spa' +
        'ce to add input files generated outside of ModelMuse.)'#13#10'Unit num' +
        'bers 70-95 are available for the user.'
    end
    object memoLines: TMemo
      Left = 8
      Top = 72
      Width = 500
      Height = 138
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      ExplicitWidth = 496
      ExplicitHeight = 137
    end
    object cbFlowPackage: TCheckBox
      Left = 8
      Top = 216
      Width = 500
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Flow package other than BCF6, LPF, HUF2, or UPW included'
      TabOrder = 1
      ExplicitTop = 215
      ExplicitWidth = 496
    end
    object cbSolvers: TCheckBox
      Left = 8
      Top = 239
      Width = 500
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Solver other than PCG, PCGN, GMG, SIP, DE4, or NWT included'
      TabOrder = 2
      ExplicitTop = 238
      ExplicitWidth = 496
    end
    object btnHelp: TBitBtn
      Left = 250
      Top = 260
      Width = 82
      Height = 34
      Anchors = [akRight, akBottom]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
      ExplicitLeft = 246
      ExplicitTop = 259
    end
    object btnOK: TBitBtn
      Left = 338
      Top = 260
      Width = 82
      Height = 34
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnOKClick
      ExplicitLeft = 334
      ExplicitTop = 259
    end
    object btnCancel: TBitBtn
      Left = 426
      Top = 260
      Width = 82
      Height = 34
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 5
      ExplicitLeft = 422
      ExplicitTop = 259
    end
    object btnFileTypes: TButton
      Left = 8
      Top = 262
      Width = 137
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Show file types'
      TabOrder = 6
      OnClick = btnFileTypesClick
      ExplicitTop = 261
    end
  end
  object pnlModel: TPanel
    Left = 0
    Top = 0
    Width = 520
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 516
    DesignSize = (
      520
      41)
    object lblModel: TLabel
      Left = 465
      Top = 11
      Width = 43
      Height = 18
      Anchors = [akTop, akRight]
      Caption = 'Model'
      ExplicitLeft = 432
    end
    object comboModel: TComboBox
      Left = 8
      Top = 8
      Width = 451
      Height = 26
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = comboModelChange
      ExplicitWidth = 447
    end
  end
end
