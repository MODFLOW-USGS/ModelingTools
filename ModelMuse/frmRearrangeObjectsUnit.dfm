inherited frmRearrangeObjects: TfrmRearrangeObjects
  Left = 562
  Top = 235
  HelpType = htKeyword
  HelpKeyword = 'Rearrange_Objects_Dialog_Box'
  ActiveControl = sgObjects
  Caption = 'Rearrange Objects'
  ClientHeight = 400
  ClientWidth = 321
  ExplicitWidth = 339
  ExplicitHeight = 445
  PixelsPerInch = 120
  TextHeight = 18
  object sgObjects: TStringGrid
    Left = 0
    Top = 105
    Width = 321
    Height = 196
    Align = alClient
    ColCount = 2
    DefaultColWidth = 20
    FixedColor = 14803425
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowMoving, goEditing]
    TabOrder = 1
    OnDrawCell = sgObjectsDrawCell
    OnMouseDown = sgObjectsMouseDown
    OnMouseMove = sgObjectsMouseMove
    OnMouseUp = sgObjectsMouseUp
    OnMouseWheelDown = sgObjectsMouseWheelDown
    OnMouseWheelUp = sgObjectsMouseWheelUp
    OnSelectCell = sgObjectsSelectCell
    ExplicitWidth = 311
    ExplicitHeight = 192
    ColWidths = (
      20
      20)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 301
    Width = 321
    Height = 99
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 297
    ExplicitWidth = 311
    DesignSize = (
      321
      99)
    object btnCancel: TBitBtn
      Left = 222
      Top = 55
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 3
      ExplicitLeft = 212
    end
    object btnOK: TBitBtn
      Left = 125
      Top = 55
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
      ExplicitLeft = 115
    end
    object btnHelp: TBitBtn
      Left = 28
      Top = 55
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
      ExplicitLeft = 18
    end
    object rgShow: TRadioGroup
      Left = 8
      Top = 6
      Width = 305
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
      ExplicitWidth = 295
    end
  end
  object pnlInstructions: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 105
    Align = alTop
    ParentColor = True
    TabOrder = 0
    ExplicitWidth = 311
    DesignSize = (
      321
      105)
    object lblInstructions: TLabel
      Left = 8
      Top = 0
      Width = 286
      Height = 90
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Click to the left of the name of an object and drag to a new pos' +
        'ition.  You can also type a new name for an object.'#13#10#13#10'Objects a' +
        're listed from back to front.'
      WordWrap = True
    end
  end
end
