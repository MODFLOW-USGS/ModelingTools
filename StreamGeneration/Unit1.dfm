object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 317
  ClientWidth = 819
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 1
    OnClick = btn1Click
  end
  object pgc1: TPageControl
    Left = 89
    Top = 0
    Width = 730
    Height = 317
    ActivePage = tabFinalSpillPoints
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabElevations: TTabSheet
      Caption = 'Elevations'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1042
      ExplicitHeight = 0
      object sgElevations: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
    object tabFlowDirections: TTabSheet
      Caption = 'FlowDirections'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1042
      ExplicitHeight = 0
      object sgFlowDirections: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
    object tabNeighbors: TTabSheet
      Caption = 'Neighbors'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1042
      ExplicitHeight = 0
      object sgNeighbors: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        ColCount = 8
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
    object tabEndpoints: TTabSheet
      Caption = 'Endpoints'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1042
      ExplicitHeight = 0
      object sgEndPoints: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
    object tabEdgePoints: TTabSheet
      Caption = 'EdgePoints'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1042
      ExplicitHeight = 0
      object sgEdgePointsBoolean: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
    object tabSpillPoints: TTabSheet
      Caption = 'SpillPoints'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 945
      ExplicitHeight = 0
      object sgSpillPoints: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
    object tabNewFlowDir: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'NewFlowDir'
      ImageIndex = 6
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1042
      ExplicitHeight = 0
      object sgNewFlowDir: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
    object tabNewEndPoints: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'NewEndPoints'
      ImageIndex = 7
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1042
      ExplicitHeight = 0
      object sgNewEndPoints: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
    object tabPitless: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Pitless'
      ImageIndex = 8
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1042
      ExplicitHeight = 0
      object sgPitless: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
    object tabFinalFlowDir: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'FinalFlowDir'
      ImageIndex = 9
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 1042
      ExplicitHeight = 0
      object sgFinalFlowdir: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
    object tabFinalSpillPoints: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'FinalSpillPoints'
      ImageIndex = 10
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object sgFinalSpillPoints: TStringGrid
        Left = 0
        Top = 0
        Width = 723
        Height = 293
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        TabOrder = 0
      end
    end
  end
  object edRandSeed: TEdit
    Left = 6
    Top = 86
    Width = 78
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    TabOrder = 2
    Text = 'edRandSeed'
  end
  object dlgSave1: TSaveDialog
    DefaultExt = '.grd'
    Filter = 'SurferGridFiles (*.grd)|*.grd'
    Left = 24
    Top = 48
  end
end
