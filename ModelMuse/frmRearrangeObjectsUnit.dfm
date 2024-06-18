inherited frmRearrangeObjects: TfrmRearrangeObjects
  Left = 562
  Top = 235
  HelpType = htKeyword
  HelpKeyword = 'Rearrange_Objects_Dialog_Box'
  Caption = 'Rearrange Objects'
  ClientHeight = 400
  ClientWidth = 428
  ExplicitWidth = 444
  ExplicitHeight = 439
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 301
    Width = 428
    Height = 99
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 300
    ExplicitWidth = 424
    DesignSize = (
      428
      99)
    object btnCancel: TBitBtn
      Left = 321
      Top = 55
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 3
      ExplicitLeft = 317
    end
    object btnOK: TBitBtn
      Left = 224
      Top = 55
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
      ExplicitLeft = 220
    end
    object btnHelp: TBitBtn
      Left = 127
      Top = 55
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
      ExplicitLeft = 123
    end
    object rgShow: TRadioGroup
      Left = 8
      Top = 6
      Width = 404
      Height = 43
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'All'
        'Visible'
        'Selected')
      TabOrder = 0
      OnClick = rgShowClick
      ExplicitWidth = 400
    end
  end
  object pnlInstructions: TPanel
    Left = 0
    Top = 0
    Width = 428
    Height = 105
    Align = alTop
    ParentColor = True
    TabOrder = 0
    ExplicitWidth = 424
    DesignSize = (
      428
      105)
    object lblInstructions: TLabel
      Left = 8
      Top = 0
      Width = 357
      Height = 90
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Click to the left of the name of an object and drag to a new pos' +
        'ition.  You can also type a new name for an object.'#13#10#13#10'Objects a' +
        're listed from back to front.'
      WordWrap = True
    end
  end
  object sgObjects: TRbwDataGrid4
    Left = 0
    Top = 105
    Width = 428
    Height = 196
    Align = alClient
    ColCount = 3
    DefaultColWidth = 20
    FixedCols = 1
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowMoving, goEditing, goAlwaysShowEditor]
    TabOrder = 2
    OnDrawCell = sgObjectsDrawCell
    OnExit = sgObjectsExit
    OnMouseDown = sgObjectsMouseDown
    OnMouseMove = sgObjectsMouseMove
    OnMouseUp = sgObjectsMouseUp
    OnMouseWheelDown = sgObjectsMouseWheelDown
    OnMouseWheelUp = sgObjectsMouseWheelUp
    OnSelectCell = sgObjectsSelectCell
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
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
        Format = rcf4String
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
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 20
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4String
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = False
      end
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
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end>
    WordWrapRowCaptions = False
    ExplicitWidth = 424
    ExplicitHeight = 195
    RowHeights = (
      61
      24
      24
      24
      24)
  end
end
