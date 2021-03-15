inherited frameContourData: TframeContourData
  Width = 578
  Height = 501
  ExplicitWidth = 578
  ExplicitHeight = 501
  inherited pcChoices: TPageControl
    Width = 578
    Height = 501
    ExplicitWidth = 578
    ExplicitHeight = 501
    inherited tabSelection: TTabSheet
      ExplicitWidth = 570
      ExplicitHeight = 473
      DesignSize = (
        570
        473)
      inherited lblColorScheme: TLabel
        Left = 9
        Top = 327
        Anchors = [akLeft, akRight, akBottom]
        ExplicitLeft = 9
        ExplicitTop = 324
      end
      inherited lblCycles: TLabel
        Left = 485
        Top = 327
        ExplicitLeft = 485
        ExplicitTop = 324
      end
      inherited pbColorScheme: TPaintBox
        Left = 8
        Top = 374
        Width = 405
        ExplicitLeft = 8
        ExplicitTop = 269
        ExplicitWidth = 405
      end
      inherited lblColorAdjustment: TLabel
        Top = 413
        Anchors = [akLeft, akRight, akBottom]
        ExplicitTop = 410
      end
      object lblAlgorithm: TLabel [6]
        Left = 8
        Top = 191
        Width = 36
        Height = 13
        Caption = 'Method'
      end
      object lblContourInterval: TLabel [7]
        Left = 444
        Top = 60
        Width = 80
        Height = 13
        Caption = 'Contour Interval'
      end
      object lblSpacing: TLabel [8]
        Left = 347
        Top = 245
        Width = 102
        Height = 13
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Label spacing (pixels)'
      end
      inherited comboColorScheme: TComboBox
        Top = 344
        Width = 471
        TabOrder = 12
        ExplicitTop = 344
        ExplicitWidth = 471
      end
      inherited seCycles: TJvSpinEdit
        Left = 482
        Top = 346
        Width = 85
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 13
        ExplicitLeft = 482
        ExplicitTop = 346
        ExplicitWidth = 85
      end
      inherited jsColorExponent: TJvxSlider
        Top = 428
        TabOrder = 14
      end
      inherited seColorExponent: TJvSpinEdit
        Top = 428
        TabOrder = 15
        ExplicitTop = 428
      end
      inherited cbLogTransform: TCheckBox
        Left = 231
        Top = 432
        TabOrder = 16
        OnClick = cbLogTransformClick
        ExplicitLeft = 231
        ExplicitTop = 432
      end
      inherited udDataSets: TJvUpDown
        Left = 319
        Top = 20
        TabOrder = 2
        ExplicitLeft = 319
        ExplicitTop = 20
      end
      inherited rgUpdateLimitChoice: TRadioGroup
        Top = 248
        Width = 331
        TabOrder = 9
        ExplicitTop = 248
        ExplicitWidth = 331
      end
      inherited virttreecomboDataSets: TRbwStringTreeCombo
        Top = 26
        Width = 305
        Tree.OnGetNodeDataSize = virttreecomboDataSetsTreeGetNodeDataSize
        TabOrder = 1
        ExplicitTop = 26
        ExplicitWidth = 305
      end
      inherited reComment: TRichEdit
        Width = 559
        Height = 105
        TabOrder = 5
        ExplicitWidth = 559
        ExplicitHeight = 105
      end
      inherited btnColorSchemes: TButton
        Left = 347
        Top = 297
        TabOrder = 11
        ExplicitLeft = 347
        ExplicitTop = 297
      end
      object btnEditContours: TButton
        Left = 444
        Top = 20
        Width = 119
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Edit contours...'
        Enabled = False
        TabOrder = 3
        OnClick = btnEditContoursClick
      end
      object cbSpecifyContours: TJvCheckBox
        Left = 347
        Top = 9
        Width = 96
        Height = 41
        Anchors = [akTop, akRight]
        Caption = 'Specify contours'
        TabOrder = 0
        WordWrap = True
        OnClick = cbSpecifyContoursClick
        LinkedControls = <>
        AutoSize = False
      end
      object cbLabelContours: TCheckBox
        Left = 347
        Top = 191
        Width = 202
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Label contours'
        TabOrder = 6
        OnClick = cbLabelContoursClick
      end
      object btnContourFont: TButton
        Left = 347
        Top = 214
        Width = 153
        Height = 25
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Contour label font'
        Enabled = False
        TabOrder = 8
        OnClick = btnContourFontClick
      end
      object comboAlgorithm: TComboBox
        Left = 3
        Top = 210
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 7
        Text = 'Simple'
        Items.Strings = (
          'Simple'
          'ACM 626')
      end
      object rdeContourInterval: TRbwDataEntry
        Left = 347
        Top = 56
        Width = 94
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seLabelSpacing: TJvSpinEdit
        Left = 347
        Top = 264
        Width = 100
        Height = 21
        Increment = 20.000000000000000000
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 10
      end
    end
    inherited tabFilters: TTabSheet
      ExplicitWidth = 570
      ExplicitHeight = 473
      DesignSize = (
        570
        473)
      inherited lblNumberOfValuesToIgnore: TLabel
        Top = 449
        ExplicitTop = 441
      end
      object lblModel: TLabel [5]
        Left = 203
        Top = 152
        Width = 86
        Height = 13
        Caption = 'Models to contour'
      end
      inherited rdgValuesToIgnore: TRbwDataGrid4
        Height = 325
        ExplicitHeight = 325
      end
      inherited seNumberOfValuesToIgnore: TJvSpinEdit
        Top = 446
        ExplicitTop = 446
      end
      object clbxModel: TJvCheckListBox
        Left = 203
        Top = 171
        Width = 121
        Height = 150
        OnClickCheck = clbxModelClickCheck
        DoubleBuffered = False
        ItemHeight = 13
        ParentDoubleBuffered = False
        TabOrder = 6
      end
    end
    inherited tabLegend: TTabSheet
      ExplicitWidth = 570
      ExplicitHeight = 473
      inherited imLegend: TImage
        Width = 364
        Height = 473
        ExplicitWidth = 364
        ExplicitHeight = 471
      end
      inherited splColor: TSplitter
        Height = 473
        ExplicitHeight = 368
      end
      inherited pnlLegend: TPanel
        Height = 473
        ExplicitHeight = 473
        DesignSize = (
          201
          473)
        inherited lblColorLegendRows: TLabel
          Top = 412
          ExplicitTop = 410
        end
        inherited seLegendRows: TJvSpinEdit
          Top = 433
          ExplicitTop = 433
        end
        inherited rdgLegend: TRbwDataGrid4
          Height = 347
          ExplicitHeight = 347
        end
      end
    end
  end
  inherited dlgFontLegend: TFontDialog
    Top = 376
  end
  object fdContourFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 480
    Top = 144
  end
end
