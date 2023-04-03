inherited frmImportSutraModelResults: TfrmImportSutraModelResults
  HelpType = htKeyword
  HelpKeyword = 'Import_SUTRA_Model_Results_Dia'
  Caption = 'Import SUTRA Model Results'
  ClientHeight = 491
  ClientWidth = 417
  ExplicitWidth = 429
  ExplicitHeight = 529
  TextHeight = 18
  object lblTimeStepsToImport: TLabel
    Left = 223
    Top = 8
    Width = 141
    Height = 18
    Caption = 'Time steps to import'
  end
  object lblDataToImport: TLabel
    Left = 8
    Top = 8
    Width = 99
    Height = 18
    Caption = 'Data to import'
  end
  object chklstTimeStepsToImport: TCheckListBox
    Left = 223
    Top = 29
    Width = 182
    Height = 314
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 18
    TabOrder = 1
    OnClick = chklstTimeStepsToImportClick
    ExplicitWidth = 178
    ExplicitHeight = 313
  end
  object chklstDataToImport: TCheckListBox
    Left = 8
    Top = 29
    Width = 209
    Height = 211
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 18
    Items.Strings = (
      'Pressure'
      'Concentration or temperature'
      'Saturation'
      'Liquid saturation'
      'Ice saturation'
      'X velocity'
      'Y velocity'
      'Z velocity'
      'X Darcy velocity'
      'Y Darcy velocity'
      'Z Darcy velocity')
    TabOrder = 0
    OnClick = chklstDataToImportClick
    ExplicitHeight = 210
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 350
    Width = 417
    Height = 141
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 6
    ExplicitTop = 349
    ExplicitWidth = 413
    DesignSize = (
      417
      141)
    object lblColorMesh: TLabel
      Left = 8
      Top = 50
      Width = 153
      Height = 18
      Caption = 'Color or contour mesh'
    end
    object btnHelp: TBitBtn
      Left = 146
      Top = 103
      Width = 82
      Height = 30
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnHelpClick
      ExplicitLeft = 142
    end
    object btnOK: TBitBtn
      Left = 234
      Top = 103
      Width = 82
      Height = 30
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnOKClick
      ExplicitLeft = 230
    end
    object btnCancel: TBitBtn
      Left = 322
      Top = 103
      Width = 83
      Height = 30
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 4
      ExplicitLeft = 318
    end
    object rgDisplayChoice: TRadioGroup
      Left = 8
      Top = 2
      Width = 397
      Height = 42
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Display choice'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Color mesh'
        'Contour mesh'
        'Neither')
      TabOrder = 0
      ExplicitWidth = 393
    end
    object comboColorMesh: TComboBox
      Left = 8
      Top = 71
      Width = 397
      Height = 26
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      ExplicitWidth = 393
    end
  end
  object btnSelectAll: TButton
    Left = 8
    Top = 246
    Width = 104
    Height = 47
    Anchors = [akLeft, akBottom]
    Caption = 'Select all data sets'
    TabOrder = 2
    WordWrap = True
    OnClick = btnSelectAllClick
    ExplicitTop = 245
  end
  object btnSelectNone: TButton
    Left = 8
    Top = 297
    Width = 104
    Height = 47
    Anchors = [akLeft, akBottom]
    Caption = 'Deselect all data sets'
    TabOrder = 4
    WordWrap = True
    OnClick = btnSelectNoneClick
    ExplicitTop = 296
  end
  object btnSelectAllTimes: TButton
    Left = 113
    Top = 246
    Width = 104
    Height = 47
    Anchors = [akLeft, akBottom]
    Caption = 'Select all times'
    TabOrder = 3
    WordWrap = True
    OnClick = btnSelectAllTimesClick
    ExplicitTop = 245
  end
  object btnDeselectAllTimes: TButton
    Left = 113
    Top = 297
    Width = 104
    Height = 47
    Anchors = [akLeft, akBottom]
    Caption = 'Deselect all times'
    TabOrder = 5
    WordWrap = True
    OnClick = btnDeselectAllTimesClick
    ExplicitTop = 296
  end
  object dlgOpenSutraFile: TJvOpenDialog
    DefaultExt = '.nod'
    Filter = 
      'SUTRA output files (*.nod; *.ele; *.bcof; *.bcos; *.bcop; *.bcou' +
      '; *.lkst)|*.nod;*.ele;*.bcof;*.bcos;*.bcop;*.bcou;*.rst;*.lkst|S' +
      'UTRA node and element files (*.nod; *.ele)|*.nod;*.ele|Fluid sou' +
      'rces and sinks (*.bcof)|*.bcof|Solute or energy sources and sink' +
      's (*.bcos)|*.bcos|Specified pressure (*.bcop)|*.bcop|Specified c' +
      'oncentration or temperature (*.bcou)|*.bcou|Restart Files (*.rst' +
      ')|*.rst|Lake stage and depth (*.lkst)|*.lkst'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    OnTypeChange = dlgOpenSutraFileTypeChange
    Height = 0
    Width = 0
    Left = 128
    Top = 104
  end
end
