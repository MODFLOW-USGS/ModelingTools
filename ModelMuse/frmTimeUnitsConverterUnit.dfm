inherited frmTimeUnitsConverter: TfrmTimeUnitsConverter
  HelpType = htKeyword
  HelpKeyword = 'Time_Units_Converter'
  Caption = 'Time Units Converter'
  ClientHeight = 398
  ClientWidth = 485
  OnClose = FormClose
  ExplicitWidth = 503
  ExplicitHeight = 443
  PixelsPerInch = 120
  TextHeight = 18
  object pnlButtons: TPanel
    Left = 0
    Top = 357
    Width = 485
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      485
      41)
    object btnOK: TBitBtn
      Left = 152
      Top = 2
      Width = 220
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'Copy output to clipboard'
      Default = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
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
      Left = 378
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnHelp: TBitBtn
      Left = 55
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
  inline frameTimeGrid: TframeGrid
    Left = 0
    Top = 65
    Width = 485
    Height = 292
    Align = alClient
    TabOrder = 1
    ExplicitTop = 65
    ExplicitWidth = 485
    ExplicitHeight = 292
    inherited Panel: TPanel
      Top = 251
      Width = 485
      ExplicitTop = 251
      ExplicitWidth = 485
      inherited lbNumber: TLabel
        Width = 55
        Height = 18
        ExplicitWidth = 55
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 252
        ExplicitLeft = 252
      end
      inherited sbInsert: TSpeedButton
        Left = 298
        ExplicitLeft = 298
      end
      inherited sbDelete: TSpeedButton
        Left = 345
        ExplicitLeft = 345
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 485
      Height = 251
      ColCount = 2
      OnSetEditText = frameTimeGridGridSetEditText
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
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -13
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
        end>
      ExplicitWidth = 485
      ExplicitHeight = 251
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 485
    Height = 65
    Align = alTop
    TabOrder = 2
    object lblInputUnits: TLabel
      Left = 8
      Top = 7
      Width = 68
      Height = 18
      Caption = 'Input units'
    end
    object lblOutputUnits: TLabel
      Left = 111
      Top = 7
      Width = 81
      Height = 18
      Caption = 'Output units'
    end
    object comboInputUnits: TComboBox
      Left = 8
      Top = 28
      Width = 97
      Height = 26
      Style = csDropDownList
      TabOrder = 0
      OnChange = comboUnitsChange
      Items.Strings = (
        'seconds'
        'minutes'
        'hours'
        'days'
        'years')
    end
    object comboOutputUnits: TComboBox
      Left = 111
      Top = 28
      Width = 97
      Height = 26
      Style = csDropDownList
      TabOrder = 1
      OnChange = comboUnitsChange
      Items.Strings = (
        'seconds'
        'minutes'
        'hours'
        'days'
        'years')
    end
  end
end
