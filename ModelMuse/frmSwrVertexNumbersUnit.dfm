inherited frmSwrVertexNumbers: TfrmSwrVertexNumbers
  HelpType = htKeyword
  HelpKeyword = 'SWR_Vertex_Numbers_Dialog_Box'
  Caption = 'SWR Reach Numbers'
  ClientWidth = 356
  ExplicitWidth = 374
  ExplicitHeight = 271
  PixelsPerInch = 120
  TextHeight = 18
  inline frameVertexNumbers: TframeGrid
    Left = 0
    Top = 0
    Width = 356
    Height = 184
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 424
    ExplicitHeight = 184
    inherited Panel: TPanel
      Top = 143
      Width = 356
      ExplicitTop = 143
      ExplicitWidth = 424
      inherited lbNumber: TLabel
        Width = 132
        Height = 18
        Caption = 'Number of reaches'
        ExplicitWidth = 132
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 254
        ExplicitLeft = 254
      end
      inherited sbInsert: TSpeedButton
        Left = 283
        ExplicitLeft = 220
      end
      inherited sbDelete: TSpeedButton
        Left = 312
        ExplicitLeft = 312
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 356
      Height = 143
      Columns = <
        item
          AutoAdjustRowHeights = False
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
      ExplicitWidth = 424
      ExplicitHeight = 143
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 184
    Width = 356
    Height = 42
    Align = alBottom
    TabOrder = 1
    ExplicitWidth = 424
    DesignSize = (
      356
      42)
    object btnHelp: TBitBtn
      Left = 85
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      ExplicitLeft = 153
    end
    object btnOK: TBitBtn
      Left = 174
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
      ExplicitLeft = 242
    end
    object btnCancel: TBitBtn
      Left = 263
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
      ExplicitLeft = 331
    end
  end
end
