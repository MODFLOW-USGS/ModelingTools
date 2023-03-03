inherited frmImportVertexValues: TfrmImportVertexValues
  HelpType = htKeyword
  HelpKeyword = 'Import-Vertex-Values-Dialog-Bo'
  Caption = 'Import Vertex Values'
  ClientWidth = 697
  ExplicitWidth = 713
  TextHeight = 18
  object Splitter1: TSplitter
    Left = 113
    Top = 0
    Width = 5
    Height = 193
  end
  object pnlBase: TPanel
    Left = 0
    Top = 193
    Width = 697
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 426
    DesignSize = (
      697
      41)
    object btnHelp: TBitBtn
      Left = 352
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 81
    end
    object btnOK: TBitBtn
      Left = 466
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 195
    end
    object btnCancel: TBitBtn
      Left = 580
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 309
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 113
    Height = 193
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 1
    object memoKeys: TMemo
      Left = 1
      Top = 25
      Width = 111
      Height = 167
      Align = alClient
      TabOrder = 0
      OnChange = memoKeysChange
      ExplicitTop = 24
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 111
      Height = 24
      Align = alTop
      Caption = 'Keys'
      TabOrder = 1
    end
  end
  inline frameValues: TframeGrid
    Left = 118
    Top = 0
    Width = 579
    Height = 193
    Align = alClient
    TabOrder = 2
    ExplicitLeft = 118
    ExplicitWidth = 308
    ExplicitHeight = 193
    inherited Panel: TPanel
      Top = 152
      Width = 579
      ExplicitTop = 152
      ExplicitWidth = 308
      inherited lbNumber: TLabel
        Width = 120
        Height = 18
        Caption = 'Number of values'
        ExplicitWidth = 120
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 416
        ExplicitLeft = 262
      end
      inherited sbInsert: TSpeedButton
        Left = 472
        ExplicitLeft = 298
      end
      inherited sbDelete: TSpeedButton
        Left = 527
        ExplicitLeft = 333
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 579
      Height = 152
      ColCount = 2
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
          CheckMin = False
          ComboUsed = False
          Format = rcf4Real
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -12
          ButtonFont.Name = 'Segoe UI'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4Real
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      ExplicitWidth = 308
      ExplicitHeight = 152
    end
  end
end
