inherited frmGoTo: TfrmGoTo
  Left = 527
  Top = 253
  HelpType = htKeyword
  HelpKeyword = 'Go_To_Dialog_Box'
  ActiveControl = pcMain
  Caption = 'Go To'
  ClientHeight = 320
  ClientWidth = 297
  ExplicitWidth = 313
  ExplicitHeight = 359
  PixelsPerInch = 96
  TextHeight = 18
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 297
    Height = 238
    ActivePage = tabDISV
    Align = alClient
    TabOrder = 0
    OnChange = pcMainChange
    object tabPosition: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Position_Tab'
      Caption = 'Position'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        289
        205)
      object lblX: TLabel
        Left = 266
        Top = 11
        Width = 11
        Height = 18
        Anchors = [akTop]
        Caption = 'X'
        ExplicitLeft = 271
      end
      object lblY: TLabel
        Left = 266
        Top = 52
        Width = 9
        Height = 18
        Anchors = [akTop]
        Caption = 'Y'
        ExplicitLeft = 271
      end
      object lblZ: TLabel
        Left = 266
        Top = 92
        Width = 9
        Height = 18
        Anchors = [akTop]
        Caption = 'Z'
        ExplicitLeft = 271
      end
      object lblXPrime: TLabel
        Left = 266
        Top = 132
        Width = 14
        Height = 18
        Anchors = [akTop]
        Caption = 'X'#39
        ExplicitLeft = 271
      end
      object lblYPrime: TLabel
        Left = 266
        Top = 172
        Width = 12
        Height = 18
        Anchors = [akTop]
        Caption = 'Y'#39
        ExplicitLeft = 271
      end
      object rdeX: TRbwDataEntry
        Left = 8
        Top = 8
        Width = 251
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeY: TRbwDataEntry
        Left = 8
        Top = 48
        Width = 251
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeZ: TRbwDataEntry
        Left = 8
        Top = 88
        Width = 251
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeXPrime: TRbwDataEntry
        Left = 8
        Top = 128
        Width = 251
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeYPrime: TRbwDataEntry
        Left = 8
        Top = 168
        Width = 251
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
    end
    object tabCell: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Element_Tab'
      Caption = 'Element'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        289
        205)
      object lblCol: TLabel
        Left = 232
        Top = 41
        Width = 53
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Column'
        ExplicitLeft = 221
      end
      object lblRow: TLabel
        Left = 232
        Top = 73
        Width = 31
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Row'
        ExplicitLeft = 221
      end
      object lblLay: TLabel
        Left = 232
        Top = 105
        Width = 39
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Layer'
        ExplicitLeft = 221
      end
      object lblModel: TLabel
        Left = 232
        Top = 12
        Width = 43
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Model'
        ExplicitLeft = 221
      end
      object seCol: TJvSpinEdit
        Left = 14
        Top = 42
        Width = 212
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 1
      end
      object seRow: TJvSpinEdit
        Left = 14
        Top = 72
        Width = 212
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 2
      end
      object seLayer: TJvSpinEdit
        Left = 14
        Top = 104
        Width = 212
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 3
      end
      object comboModel: TComboBox
        Left = 3
        Top = 9
        Width = 223
        Height = 26
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = comboModelChange
      end
    end
    object tabMesh: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Mesh_Tab'
      Caption = 'Mesh'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblNumber: TLabel
        Left = 130
        Top = 115
        Width = 55
        Height = 18
        Caption = 'Number'
      end
      object rgNodeElement: TRadioGroup
        Left = 3
        Top = 16
        Width = 185
        Height = 81
        Caption = 'Node or element'
        ItemIndex = 0
        Items.Strings = (
          'Node'
          'Element')
        TabOrder = 0
        OnClick = rgNodeElementClick
      end
      object seNumber: TJvSpinEdit
        Left = 3
        Top = 112
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 1
      end
    end
    object tabDISV: TTabSheet
      Caption = 'DISV'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        289
        205)
      object lblDisvCell: TLabel
        Left = 232
        Top = 6
        Width = 27
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Cell'
      end
      object lblDisvLayer: TLabel
        Left = 232
        Top = 38
        Width = 39
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Layer'
      end
      object seDisvLayer: TJvSpinEdit
        Left = 14
        Top = 35
        Width = 212
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 0
      end
      object seDisvCell: TJvSpinEdit
        Left = 14
        Top = 3
        Width = 212
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 1
      end
    end
    object tabObject: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Object_Tab'
      Caption = 'Object'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvScreenObjects: TListView
        Left = 0
        Top = 0
        Width = 289
        Height = 164
        Align = alClient
        Columns = <
          item
            Width = 256
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object pnlObject: TPanel
        Left = 0
        Top = 164
        Width = 289
        Height = 41
        Align = alBottom
        ParentColor = True
        TabOrder = 1
        object cbSelectObject: TCheckBox
          Left = 8
          Top = 8
          Width = 185
          Height = 31
          Caption = 'Select object'
          TabOrder = 0
        end
      end
    end
    object tabImage: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Image_Tab'
      Caption = 'Image'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvImages: TListView
        Left = 0
        Top = 0
        Width = 289
        Height = 205
        Align = alClient
        Columns = <
          item
            Width = 256
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 238
    Width = 297
    Height = 82
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    object btnCancel: TBitBtn
      Left = 197
      Top = 40
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 5
    end
    object btnOK: TBitBtn
      Left = 101
      Top = 40
      Width = 91
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnOKClick
    end
    object cbSide: TCheckBox
      Left = 208
      Top = 0
      Width = 73
      Height = 31
      Caption = 'Side'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbClick
    end
    object cbFront: TCheckBox
      Left = 108
      Top = 0
      Width = 85
      Height = 31
      Caption = 'Front'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = cbClick
    end
    object cbTop: TCheckBox
      Left = 4
      Top = 0
      Width = 73
      Height = 31
      Caption = 'Top'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbClick
    end
    object btnHelp: TBitBtn
      Left = 4
      Top = 40
      Width = 91
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
    end
  end
end
