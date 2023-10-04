inherited frmTimeSeries: TfrmTimeSeries
  HelpType = htKeyword
  HelpKeyword = 'Time_Series_Dialog_Box'
  Caption = 'Time Series'
  ClientHeight = 560
  ClientWidth = 624
  ExplicitWidth = 636
  ExplicitHeight = 598
  TextHeight = 18
  object tvTimeSeries: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 519
    PageDefault = 0
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnChange = tvTimeSeriesChange
    Items.Links = {00000000}
    ExplicitHeight = 518
  end
  object plTimeSeries: TJvPageList
    Left = 121
    Top = 0
    Width = 503
    Height = 519
    PropagateEnable = False
    Align = alClient
    ExplicitWidth = 499
    ExplicitHeight = 518
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 519
    Width = 624
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 518
    ExplicitWidth = 620
    DesignSize = (
      624
      41)
    object btnHelp: TBitBtn
      Left = 299
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 295
    end
    object btnOK: TBitBtn
      Left = 396
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
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
      ExplicitLeft = 392
    end
    object btnCancel: TBitBtn
      Left = 492
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 488
    end
    object btnAddGroup: TButton
      Left = 9
      Top = 6
      Width = 89
      Height = 25
      Caption = 'Add Group'
      TabOrder = 3
      OnClick = btnAddGroupClick
    end
    object btnDeleteGroup: TButton
      Left = 104
      Top = 6
      Width = 105
      Height = 25
      Caption = 'Delete Group'
      TabOrder = 4
      OnClick = btnDeleteGroupClick
    end
  end
end
