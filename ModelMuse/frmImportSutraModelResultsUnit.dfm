inherited frmImportSutraModelResults: TfrmImportSutraModelResults
  HelpType = htKeyword
  HelpKeyword = 'Import_SUTRA_Model_Results_Dia'
  Caption = 'Import SUTRA Model Results'
  ClientHeight = 417
  ClientWidth = 417
  ExplicitWidth = 433
  ExplicitHeight = 455
  PixelsPerInch = 120
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
    Width = 186
    Height = 240
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 18
    TabOrder = 1
    OnClick = chklstTimeStepsToImportClick
  end
  object chklstDataToImport: TCheckListBox
    Left = 8
    Top = 29
    Width = 209
    Height = 137
    ItemHeight = 18
    Items.Strings = (
      'Pressure'
      'Concentration or temperature'
      'Saturation'
      'X velocity'
      'Y velocity'
      'Z velocity')
    TabOrder = 0
    OnClick = chklstDataToImportClick
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 276
    Width = 417
    Height = 141
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 6
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
      Left = 150
      Top = 103
      Width = 82
      Height = 30
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 238
      Top = 103
      Width = 82
      Height = 30
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 326
      Top = 103
      Width = 83
      Height = 30
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 4
    end
    object rgDisplayChoice: TRadioGroup
      Left = 8
      Top = 2
      Width = 401
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
    end
    object comboColorMesh: TComboBox
      Left = 8
      Top = 71
      Width = 401
      Height = 26
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object btnSelectAll: TButton
    Left = 8
    Top = 172
    Width = 104
    Height = 47
    Caption = 'Select all data sets'
    TabOrder = 2
    WordWrap = True
    OnClick = btnSelectAllClick
  end
  object btnSelectNone: TButton
    Left = 8
    Top = 223
    Width = 104
    Height = 47
    Caption = 'Deselect all data sets'
    TabOrder = 4
    WordWrap = True
    OnClick = btnSelectNoneClick
  end
  object btnSelectAllTimes: TButton
    Left = 113
    Top = 172
    Width = 104
    Height = 47
    Caption = 'Select all times'
    TabOrder = 3
    WordWrap = True
    OnClick = btnSelectAllTimesClick
  end
  object btnDeselectAllTimes: TButton
    Left = 113
    Top = 223
    Width = 104
    Height = 47
    Caption = 'Deselect all times'
    TabOrder = 5
    WordWrap = True
    OnClick = btnDeselectAllTimesClick
  end
  object dlgOpenSutraFile: TJvOpenDialog
    DefaultExt = '.nod'
    Filter = 
      'SUTRA output files (*.nod; *.ele; *.bcof; *.bcos; *.bcop; *.bcou' +
      ')|*.nod;*.ele;*.bcof;*.bcos;*.bcop;*.bcou;*.rst|SUTRA node and e' +
      'lement files (*.nod; *.ele)|*.nod;*.ele|Fluid sources and sinks ' +
      '(*.bcof)|*.bcof|Solute or energy sources and sinks (*.bcos)|*.bc' +
      'os|Specified pressure (*.bcop)|*.bcop|Specified concentration or' +
      ' temperature (*.bcou)|*.bcou|Restart Files (*.rst)|*.rst'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    OnTypeChange = dlgOpenSutraFileTypeChange
    Height = 0
    Width = 0
    Left = 128
    Top = 104
  end
end
