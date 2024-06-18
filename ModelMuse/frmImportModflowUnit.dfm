inherited frmImportModflow: TfrmImportModflow
  HelpType = htKeyword
  HelpKeyword = 'Import_MODFLOW_Model_Dialog_Box'
  Caption = 'Import MODFLOW-2005 or -NWT Model'
  ClientHeight = 444
  ClientWidth = 487
  ExplicitLeft = 3
  ExplicitTop = 3
  ExplicitWidth = 499
  ExplicitHeight = 482
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 339
    Width = 487
    Height = 105
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 338
    ExplicitWidth = 483
    DesignSize = (
      487
      105)
    object btnHelp: TBitBtn
      Left = 202
      Top = 4
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 198
    end
    object btnOK: TBitBtn
      Left = 291
      Top = 4
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Enabled = False
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 287
    end
    object btnCancel: TBitBtn
      Left = 380
      Top = 4
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 376
    end
    object pbProgress: TProgressBar
      Left = 1
      Top = 75
      Width = 485
      Height = 29
      Margins.Bottom = 0
      Align = alBottom
      TabOrder = 4
      ExplicitWidth = 481
    end
    object sbStatusBar: TStatusBar
      Left = 1
      Top = 49
      Width = 485
      Height = 26
      Panels = <>
      ParentFont = True
      SimplePanel = True
      UseSystemFont = False
      ExplicitWidth = 481
    end
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 0
    Width = 487
    Height = 339
    ActivePage = tabSubmodel
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 483
    ExplicitHeight = 338
    object tabModel: TTabSheet
      Caption = 'Model'
      DesignSize = (
        479
        306)
      object lbl1: TLabel
        Left = 8
        Top = 11
        Width = 54
        Height = 18
        Caption = 'X origin'
      end
      object lbl2: TLabel
        Left = 8
        Top = 43
        Width = 52
        Height = 18
        Caption = 'Y origin'
      end
      object lbl3: TLabel
        Left = 206
        Top = 11
        Width = 144
        Height = 18
        Caption = 'Grid angle (degrees)'
      end
      object lbl4: TLabel
        Left = 8
        Top = 75
        Width = 66
        Height = 18
        Caption = 'Name file'
      end
      object lblWarning: TLabel
        Left = 8
        Top = 159
        Width = 411
        Height = 36
        Caption = 
          'Warning: Importing a model will cause the destruction of the out' +
          'put files of the model; try importing a copy of the model. '
        WordWrap = True
      end
      object lblGridOrigin: TLabel
        Left = 8
        Top = 104
        Width = 73
        Height = 18
        Caption = 'Grid origin'
      end
      object rdeX: TRbwDataEntry
        Left = 89
        Top = 8
        Width = 96
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeY: TRbwDataEntry
        Left = 89
        Top = 40
        Width = 96
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeGridAngle: TRbwDataEntry
        Left = 356
        Top = 8
        Width = 96
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object fedNameFile: TJvFilenameEdit
        Left = 87
        Top = 72
        Width = 372
        Height = 26
        Filter = 
          'Name files (*.nam, *.mfn, *.modflow.in)|*.nam;*.mfn;*.modflow.in' +
          '|All Files (*.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = ''
        OnChange = fedNameFileChange
      end
      object cbOldStream: TCheckBox
        Left = 8
        Top = 136
        Width = 411
        Height = 17
        Caption = 'The model is MODFLOW-2005 version 1.10 or older'
        TabOrder = 5
      end
      object comboGridOrigin: TComboBox
        Left = 89
        Top = 104
        Width = 145
        Height = 26
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'Upper left'
        Items.Strings = (
          'Upper left'
          'Lower left')
      end
      object rgMethod: TRadioGroup
        Left = 8
        Top = 201
        Width = 472
        Height = 105
        Caption = 'Method of assigning values to arrays'
        ItemIndex = 0
        Items.Strings = (
          
            'Nearest point interpolation (better if you want to change the gr' +
            'id)'
          'Intersected (faster if you don'#39't change the grid)')
        TabOrder = 6
        WordWrap = True
      end
    end
    object tabSubmodel: TTabSheet
      Caption = 'Extract submodel'
      ImageIndex = 1
      object rgSubmodelChoice: TRadioGroup
        Left = 0
        Top = 0
        Width = 479
        Height = 105
        Align = alTop
        Caption = 'Submodel choice'
        ItemIndex = 0
        Items.Strings = (
          'No submodel, import entire model'
          'Submodel polygon'
          'Submodel column and row range')
        TabOrder = 0
        WordWrap = True
        OnClick = rgSubmodelChoiceClick
        ExplicitWidth = 475
      end
      object pglstBoundaySpecification: TJvPageList
        Left = 0
        Top = 105
        Width = 479
        Height = 67
        ActivePage = jvspPolygon
        PropagateEnable = False
        Align = alClient
        ExplicitWidth = 475
        ExplicitHeight = 66
        object jvspNone: TJvStandardPage
          Left = 0
          Top = 0
          Width = 479
          Height = 67
          Caption = 'jvspNone'
        end
        object jvspPolygon: TJvStandardPage
          Left = 0
          Top = 0
          Width = 479
          Height = 67
          Caption = 'jvspPolygon'
          ExplicitWidth = 475
          ExplicitHeight = 66
          DesignSize = (
            479
            67)
          object lblPolygon: TLabel
            Left = 8
            Top = 6
            Width = 208
            Height = 18
            Caption = 'Shapefile of submodel outline.'
          end
          object fedPolygon: TJvFilenameEdit
            Left = 8
            Top = 30
            Width = 464
            Height = 26
            Filter = 'Shapefile (*.shp)|*.shp'
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = ''
            ExplicitWidth = 460
          end
        end
        object jvspColRow: TJvStandardPage
          Left = 0
          Top = 0
          Width = 479
          Height = 67
          Caption = 'jvspColRow'
          object lblFirstCol: TLabel
            Left = 16
            Top = 12
            Width = 84
            Height = 18
            Caption = 'First column'
          end
          object lblLastCol: TLabel
            Left = 16
            Top = 40
            Width = 83
            Height = 18
            Caption = 'Last column'
          end
          object lblFirstRow: TLabel
            Left = 240
            Top = 12
            Width = 60
            Height = 18
            Caption = 'First row'
          end
          object lblLastRow: TLabel
            Left = 240
            Top = 40
            Width = 59
            Height = 18
            Caption = 'Last row'
          end
          object rdeFirstCol: TRbwDataEntry
            Left = 116
            Top = 9
            Width = 89
            Height = 22
            TabOrder = 0
            Text = '1'
            DataType = dtInteger
            Max = 1.000000000000000000
            Min = 1.000000000000000000
            CheckMin = True
            ChangeDisabledColor = True
          end
          object rdeLastCol: TRbwDataEntry
            Left = 116
            Top = 37
            Width = 89
            Height = 22
            TabOrder = 2
            Text = '1'
            DataType = dtInteger
            Max = 1.000000000000000000
            Min = 1.000000000000000000
            CheckMin = True
            ChangeDisabledColor = True
          end
          object rdeFirstRow: TRbwDataEntry
            Left = 311
            Top = 9
            Width = 89
            Height = 22
            TabOrder = 1
            Text = '1'
            DataType = dtInteger
            Max = 1.000000000000000000
            Min = 1.000000000000000000
            CheckMin = True
            ChangeDisabledColor = True
          end
          object rdeLastRow: TRbwDataEntry
            Left = 311
            Top = 37
            Width = 89
            Height = 22
            TabOrder = 3
            Text = '1'
            DataType = dtInteger
            Max = 1.000000000000000000
            Min = 1.000000000000000000
            CheckMin = True
            ChangeDisabledColor = True
          end
        end
      end
      object grpBoundaryConditions: TGroupBox
        Left = 0
        Top = 172
        Width = 479
        Height = 134
        Align = alBottom
        Caption = 'Submodel boundary conditions (optional)'
        TabOrder = 2
        ExplicitTop = 171
        ExplicitWidth = 475
        DesignSize = (
          479
          134)
        object lblHeads: TLabel
          Left = 8
          Top = 24
          Width = 327
          Height = 18
          Caption = 'Specified heads from full model head output file'
          Enabled = False
        end
        object lblFlows: TLabel
          Left = 8
          Top = 78
          Width = 341
          Height = 18
          Caption = 'Specified flows from full model cell-by-cell flow file'
          Enabled = False
        end
        object fedHead: TJvFilenameEdit
          Left = 8
          Top = 48
          Width = 448
          Height = 26
          Filter = 
            'Formatted head file (*.fhd)|*.fhd|Binary head file (*.bhd)|*.bhd' +
            '|Formatted head file (*.*)|*.*|Binary head file (*.*)|*.*'
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = ''
          ExplicitWidth = 444
        end
        object fedFlow: TJvFilenameEdit
          Left = 8
          Top = 102
          Width = 448
          Height = 26
          Filter = 
            'Flow Files (*.cbc, *.bud, *.cbb)|*.cbc;*.bud;*.cbb|All files (*.' +
            '*)|*.*'
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Text = ''
          ExplicitWidth = 444
        end
      end
    end
  end
end
