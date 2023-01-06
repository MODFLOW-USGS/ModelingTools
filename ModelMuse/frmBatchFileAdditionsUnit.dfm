inherited frmBatchFileAdditions: TfrmBatchFileAdditions
  HelpType = htKeyword
  HelpKeyword = 'Batch_File_Additions_Dialog'
  Caption = 'Batch File Additions'
  ClientHeight = 459
  ClientWidth = 328
  ExplicitWidth = 344
  ExplicitHeight = 498
  PixelsPerInch = 120
  TextHeight = 18
  object Panel1: TPanel
    Left = 0
    Top = 418
    Width = 328
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btnHelp: TBitBtn
      Left = 38
      Top = 4
      Width = 89
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 134
      Top = 4
      Width = 89
      Height = 33
      Caption = 'OK'
      Default = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        04000000000068010000120B0000120B00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      ModalResult = 1
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 230
      Top = 4
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 64
    Width = 328
    Height = 354
    Align = alClient
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 1
      Top = 169
      Width = 326
      Height = 5
      Cursor = crVSplit
      Align = alTop
      ExplicitLeft = 0
      ExplicitTop = 194
      ExplicitWidth = 328
    end
    inline frameBatchFileAfter: TframeBatchFileLines
      Left = 1
      Top = 174
      Width = 326
      Height = 179
      Align = alClient
      TabOrder = 0
      TabStop = True
      ExplicitLeft = 1
      ExplicitTop = 174
      ExplicitWidth = 326
      ExplicitHeight = 179
      inherited lblLines: TLabel
        Width = 249
        Height = 18
        Caption = 'Batch file lines executed after model'
        ExplicitWidth = 249
        ExplicitHeight = 18
      end
      inherited memoLines: TMemo
        Width = 326
        Height = 147
        ExplicitWidth = 326
        ExplicitHeight = 147
      end
    end
    inline frameBatchFileBefore: TframeBatchFileLines
      Left = 1
      Top = 1
      Width = 326
      Height = 168
      Align = alTop
      TabOrder = 1
      TabStop = True
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 326
      ExplicitHeight = 168
      inherited lblLines: TLabel
        Width = 263
        Height = 18
        Caption = 'Batch file lines executed before model'
        ExplicitWidth = 263
        ExplicitHeight = 18
      end
      inherited memoLines: TMemo
        Width = 326
        Height = 136
        ExplicitWidth = 326
        ExplicitHeight = 136
      end
    end
  end
  object pnlModel: TPanel
    Left = 0
    Top = 0
    Width = 328
    Height = 64
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      328
      64)
    object lblModel: TLabel
      Left = 8
      Top = 11
      Width = 43
      Height = 18
      Caption = 'Model'
    end
    object comboModel: TComboBox
      Left = 8
      Top = 32
      Width = 319
      Height = 26
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = comboModelChange
    end
  end
end
