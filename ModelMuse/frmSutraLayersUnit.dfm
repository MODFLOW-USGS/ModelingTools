inherited frmSutraLayers: TfrmSutraLayers
  HelpType = htKeyword
  HelpKeyword = 'SUTRA_Layer_Groups_Dialog_Box'
  Caption = 'Sutra Layer Groups'
  ClientHeight = 436
  ClientWidth = 585
  ExplicitWidth = 597
  ExplicitHeight = 474
  TextHeight = 18
  object spl1: TSplitter
    Left = 137
    Top = 0
    Width = 5
    Height = 395
    ExplicitLeft = 145
    ExplicitTop = 8
    ExplicitHeight = 383
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 137
    Height = 395
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 394
    DesignSize = (
      137
      395)
    object sbAddUnit: TSpeedButton
      Left = 18
      Top = 367
      Width = 23
      Height = 22
      Hint = 'Add layer group|Add a layer group below the bottom layer group.'
      Anchors = [akLeft, akBottom]
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
        CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbAddUnitClick
    end
    object sbInsertUnit: TSpeedButton
      Left = 47
      Top = 367
      Width = 23
      Height = 22
      Hint = 
        'Insert layer group|Insert a layer group above the selected layer' +
        ' group.'
      Anchors = [akLeft, akBottom]
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
        CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbInsertUnitClick
    end
    object sbDeleteUnit: TSpeedButton
      Left = 76
      Top = 367
      Width = 23
      Height = 22
      Hint = 'Delete layer group|Delete the selected layer group.'
      Anchors = [akLeft, akBottom]
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
        00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
        00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
        000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbDeleteUnitClick
    end
    object tvLayerGroups: TTreeView
      Left = 1
      Top = 1
      Width = 135
      Height = 360
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      HideSelection = False
      Indent = 20
      MultiSelect = True
      ReadOnly = True
      TabOrder = 0
      OnChange = tvLayerGroupsChange
      ExplicitHeight = 359
    end
  end
  object pnlMain: TPanel
    Left = 142
    Top = 0
    Width = 443
    Height = 395
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 439
    ExplicitHeight = 394
    object pnlTop: TPanel
      Left = 1
      Top = 1
      Width = 441
      Height = 91
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 437
      DesignSize = (
        441
        91)
      object lbl1: TLabel
        Left = 5
        Top = 3
        Width = 196
        Height = 18
        Caption = 'Layer Group (Aquifer) Name'
      end
      object lblMinimumThickness: TLabel
        Left = 111
        Top = 62
        Width = 132
        Height = 18
        Caption = 'Minimum thickness'
      end
      object edName: TRbwEdit
        Left = 5
        Top = 27
        Width = 428
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edNameChange
        ExplicitWidth = 424
      end
      object rdeMinimumThickness: TRbwDataEntry
        Left = 5
        Top = 59
        Width = 100
        Height = 22
        TabOrder = 1
        Text = '0'
        OnChange = rdeMinimumThicknessChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    inline frameDiscretization: TframeDiscretization
      Left = 1
      Top = 92
      Width = 441
      Height = 302
      Align = alClient
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 92
      ExplicitWidth = 437
      ExplicitHeight = 301
      inherited lbl1: TLabel
        Width = 148
        Height = 18
        ExplicitWidth = 148
        ExplicitHeight = 18
      end
      inherited lbl2: TLabel
        Width = 137
        Height = 18
        ExplicitWidth = 137
        ExplicitHeight = 18
      end
      inherited pnlDiscritization: TPanel
        Width = 189
        Height = 300
        ExplicitHeight = 299
        inherited spl1: TSplitter
          Left = 101
          Height = 259
          ExplicitLeft = 110
          ExplicitHeight = 335
        end
        inherited rdgSubLayerBoundaries: TRbwDataGrid4
          Width = 101
          Height = 259
          ExplicitHeight = 258
        end
        inherited pnl1: TPanel
          Width = 189
          inherited lbl3: TLabel
            Width = 189
            Height = 41
            ExplicitWidth = 179
            ExplicitHeight = 36
          end
        end
        inherited pnlPaintboxParent: TPanel
          Left = 104
          Height = 259
          ExplicitHeight = 258
          DesignSize = (
            85
            259)
          inherited pbSubLayers: TPaintBox
            Height = 227
            ExplicitHeight = 303
          end
          inherited sbInsertLine: TSpeedButton
            Top = 234
            ExplicitTop = 269
          end
          inherited sbMoveLine: TSpeedButton
            Top = 234
            ExplicitTop = 269
          end
          inherited sbDeleteLine: TSpeedButton
            Top = 234
            ExplicitTop = 269
          end
        end
      end
      inherited rgMethod: TRadioGroup
        Height = 184
        ExplicitHeight = 183
      end
    end
  end
  object pnl2: TPanel
    Left = 0
    Top = 395
    Width = 585
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 394
    ExplicitWidth = 581
    DesignSize = (
      585
      41)
    object btnHelp: TBitBtn
      Left = 240
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 236
    end
    object btnOK: TBitBtn
      Left = 354
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 350
    end
    object btnCancel: TBitBtn
      Left = 468
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 464
    end
  end
end
