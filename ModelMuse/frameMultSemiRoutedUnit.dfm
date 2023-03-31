object frameMultSemiRouted: TframeMultSemiRouted
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 121
    Top = 0
    Width = 5
    Height = 439
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 439
    Width = 640
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      640
      41)
    object lbNumber: TLabel
      Left = 79
      Top = 9
      Width = 44
      Height = 15
      Caption = 'Number'
    end
    object sbAdd: TSpeedButton
      Left = 529
      Top = 6
      Width = 23
      Height = 22
      Hint = 'Add row|Add a row below the bottom row.'
      Anchors = []
      Enabled = False
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
      OnClick = sbAddClick
    end
    object sbDelete: TSpeedButton
      Left = 598
      Top = 6
      Width = 23
      Height = 22
      Hint = 'Delete row|Delete the selected row.'
      Anchors = []
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
      OnClick = sbDeleteClick
    end
    object seNumber: TJvSpinEdit
      Left = 8
      Top = 6
      Width = 65
      Height = 23
      CheckMinValue = True
      Enabled = False
      TabOrder = 0
      OnChange = seNumberChange
    end
  end
  object Panel1: TPanel
    Left = 126
    Top = 0
    Width = 514
    Height = 439
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitLeft = 0
    ExplicitWidth = 640
    ExplicitHeight = 81
    object pnlName: TPanel
      Left = 1
      Top = 1
      Width = 512
      Height = 41
      Align = alTop
      TabOrder = 0
      ExplicitLeft = 208
      ExplicitTop = 8
      ExplicitWidth = 185
      object lblSemiRouteName: TLabel
        Left = 135
        Top = 13
        Width = 32
        Height = 15
        Caption = 'Name'
      end
      object edSemiRouteName: TEdit
        Left = 5
        Top = 12
        Width = 121
        Height = 23
        TabOrder = 0
        OnChange = edSemiRouteNameChange
      end
    end
    inline frameFarmDiversions: TframeFarmDiversion
      Left = 1
      Top = 42
      Width = 512
      Height = 396
      Align = alClient
      Enabled = False
      TabOrder = 1
      ExplicitWidth = 517
      ExplicitHeight = 238
      inherited Panel: TPanel
        Top = 355
        Width = 512
        ExplicitTop = 197
        ExplicitWidth = 517
        inherited lblLocationMethod: TLabel
          Visible = False
        end
        inherited comboMethod: TComboBox
          Visible = False
        end
      end
      inherited Grid: TRbwDataGrid4
        Width = 512
        Height = 298
        ExplicitWidth = 512
        ExplicitHeight = 298
      end
      inherited pnlTop: TPanel
        Width = 512
        ExplicitWidth = 517
      end
    end
  end
  object tvSRCollections: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 439
    Align = alLeft
    Indent = 19
    MultiSelect = True
    MultiSelectStyle = [msControlSelect, msShiftSelect]
    TabOrder = 2
    OnChange = tvSRCollectionsChange
    ExplicitLeft = 384
    ExplicitTop = 248
    ExplicitHeight = 97
  end
  object Controller: TRbwController
    ControlList = <
      item
        Control = seNumber
      end
      item
        Control = sbAdd
      end>
    Enabled = False
    Left = 80
    Top = 24
  end
end
