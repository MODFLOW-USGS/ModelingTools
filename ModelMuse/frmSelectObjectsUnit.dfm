inherited frmSelectObjects: TfrmSelectObjects
  Left = 558
  Top = 411
  HelpType = htKeyword
  HelpKeyword = 'Select_Objects_by_Name'
  ActiveControl = btnCancel
  Caption = 'Select Objects by Name'
  ClientHeight = 375
  ClientWidth = 345
  ExplicitWidth = 361
  ExplicitHeight = 413
  PixelsPerInch = 120
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 192
    Width = 345
    Height = 183
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    object lblCount: TLabel
      Left = 8
      Top = 6
      Width = 56
      Height = 18
      Caption = 'lblCount'
    end
    object btnCancel: TBitBtn
      Left = 228
      Top = 144
      Width = 108
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 8
    end
    object btnOK: TBitBtn
      Left = 116
      Top = 144
      Width = 108
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 7
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 4
      Top = 144
      Width = 108
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 6
      OnClick = btnHelpClick
    end
    object btnSelectAll: TButton
      Left = 4
      Top = 64
      Width = 108
      Height = 33
      Caption = 'Select All'
      TabOrder = 1
      OnClick = btnSelectClick
    end
    object btnSelectNone: TButton
      Left = 116
      Top = 64
      Width = 108
      Height = 33
      Caption = 'Select None'
      TabOrder = 2
      OnClick = btnSelectClick
    end
    object btnToggle: TButton
      Left = 228
      Top = 64
      Width = 108
      Height = 33
      Caption = 'Toggle'
      TabOrder = 3
      OnClick = btnToggleClick
    end
    object cbIncludeHiddenObjects: TCheckBox
      Left = 8
      Top = 27
      Width = 305
      Height = 31
      Caption = 'Include hidden objects'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbIncludeHiddenObjectsClick
    end
    object edSearchTerm: TEdit
      Left = 228
      Top = 104
      Width = 105
      Height = 26
      TabOrder = 5
      TextHint = 'Search Term'
    end
    object btnSelectByName: TButton
      Left = 4
      Top = 104
      Width = 220
      Height = 33
      Caption = 'Select Names Containing:'
      TabOrder = 4
      OnClick = btnSelectByNameClick
    end
  end
  object pcObjects: TPageControl
    Left = 0
    Top = 0
    Width = 345
    Height = 192
    ActivePage = tabTop
    Align = alClient
    TabOrder = 0
    object tabTop: TTabSheet
      Caption = 'Top'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvTop: TListView
        Left = 0
        Top = 0
        Width = 337
        Height = 159
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Width = 308
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = lvTopChange
        OnDblClick = lvTopDblClick
      end
    end
    object tabFront: TTabSheet
      Caption = 'Front'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvFront: TListView
        Left = 0
        Top = 0
        Width = 337
        Height = 159
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Width = 308
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = lvTopChange
        OnDblClick = lvTopDblClick
      end
    end
    object tabSide: TTabSheet
      Caption = 'Side'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvSide: TListView
        Left = 0
        Top = 0
        Width = 337
        Height = 159
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Width = 308
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = lvTopChange
        OnDblClick = lvTopDblClick
      end
    end
  end
end
