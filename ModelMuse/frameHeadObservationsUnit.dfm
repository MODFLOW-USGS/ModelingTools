inherited frameHeadObservations: TframeHeadObservations
  inherited pcData: TJvPageControl
    Top = 134
    Height = 216
    ActivePage = tabTimes
    ExplicitTop = 134
    ExplicitHeight = 216
    inherited tabTimes: TTabSheet
      ExplicitHeight = 188
      inherited Panel5: TPanel
        inherited rdeMultiValueEdit: TRbwDataEntry
          OnChange = rdeMultiValueEditChange
        end
        object comboMultiStatFlag: TJvImageComboBox
          Left = 99
          Top = 5
          Width = 89
          Height = 23
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          DroppedWidth = 145
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 17
          ItemIndex = -1
          TabOrder = 1
          OnChange = comboMultiStatFlagChange
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Variance'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Standard dev.'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Coef. of var.'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Weight'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Sq. rt. of weight'
            end>
        end
      end
      inherited Panel2: TPanel
        Top = 112
        ExplicitTop = 112
      end
      inherited rdgObservations: TRbwDataGrid4
        Height = 77
        ExplicitHeight = 77
      end
    end
    inherited tabLayers: TTabSheet
      ExplicitHeight = 188
      inherited Panel4: TPanel
        Top = 112
        ExplicitTop = 112
        inherited seLayers: TJvSpinEdit
          Left = 9
          ExplicitLeft = 9
        end
      end
      inherited rdgLayers: TRbwDataGrid4
        Height = 77
        ExplicitHeight = 77
      end
    end
  end
  inherited pnlName: TPanel
    Height = 109
    ExplicitHeight = 109
    object rgMultiObsMethod: TRadioGroup
      Left = 9
      Top = 49
      Width = 368
      Height = 56
      Caption = 'How will observations be analyzed? (ITT)'
      Enabled = False
      ItemIndex = 1
      Items.Strings = (
        'All heads (1)'
        'Calculate drawdown relative to first head (2)')
      TabOrder = 2
      OnClick = rgMultiObsMethodClick
    end
  end
end
