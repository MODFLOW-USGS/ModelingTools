inherited frmSwrVertexNumbers: TfrmSwrVertexNumbers
  HelpType = htKeyword
  HelpKeyword = 'SWR_Vertex_Numbers_Dialog_Box'
  Caption = 'SWR Reach Numbers'
  ClientWidth = 356
  ExplicitWidth = 368
  ExplicitHeight = 272
  PixelsPerInch = 120
  TextHeight = 18
  inline frameVertexNumbers: TframeGrid
    Left = 0
    Top = 0
    Width = 356
    Height = 192
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 352
    ExplicitHeight = 191
    inherited Panel: TPanel
      Top = 151
      Width = 356
      ExplicitTop = 150
      ExplicitWidth = 352
      inherited lbNumber: TLabel
        Width = 132
        Height = 18
        Caption = 'Number of reaches'
        ExplicitWidth = 132
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 251
        ExplicitLeft = 254
      end
      inherited sbInsert: TSpeedButton
        Left = 279
        ExplicitLeft = 220
      end
      inherited sbDelete: TSpeedButton
        Left = 309
        ExplicitLeft = 312
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 356
      Height = 151
      Columns = <
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = True
          ComboUsed = False
          Format = rcf4Integer
          LimitToList = False
          Max = 1.000000000000000000
          MaxLength = 0
          Min = 1.000000000000000000
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      ExplicitWidth = 352
      ExplicitHeight = 150
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 192
    Width = 356
    Height = 42
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 191
    ExplicitWidth = 352
    DesignSize = (
      356
      42)
    object btnHelp: TBitBtn
      Left = 81
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 77
    end
    object btnOK: TBitBtn
      Left = 170
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 166
    end
    object btnCancel: TBitBtn
      Left = 259
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 255
    end
  end
end
