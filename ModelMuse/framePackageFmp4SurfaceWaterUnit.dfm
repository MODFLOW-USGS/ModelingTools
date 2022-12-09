inherited framePackageFmp4SurfaceWater: TframePackageFmp4SurfaceWater
  Height = 450
  ExplicitHeight = 450
  DesignSize = (
    422
    450)
  object cpnlgrp1: TCategoryPanelGroup [3]
    Left = 0
    Top = 157
    Width = 422
    Height = 293
    VertScrollBar.Tracking = True
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Segoe UI'
    HeaderFont.Style = []
    TabOrder = 1
    object cpnlPrint: TCategoryPanel
      Top = 0
      Height = 153
      Caption = 'Print'
      TabOrder = 0
      object clbPrint: TCheckListBox
        Left = 0
        Top = 0
        Width = 399
        Height = 127
        Align = alClient
        Enabled = False
        ItemHeight = 15
        Items.Strings = (
          'PRINT SFR_DELIVERY'
          'PRINT SFR_DELIVERY_BY_WBS'
          'PRINT SFR_RETURN'
          'PRINT SFR_SRR_ONLY'
          'PRINT NRD'
          'PRINT NRD_BY_WBS')
        TabOrder = 0
        ExplicitLeft = 32
        ExplicitTop = 80
        ExplicitWidth = 121
        ExplicitHeight = 97
      end
    end
    object cpnlOptions: TCategoryPanel
      Top = 153
      Height = 228
      Caption = 'Options'
      Enabled = False
      TabOrder = 1
      ExplicitTop = 129
      object lblNon_Routed_Delivery: TLabel
        Left = 173
        Top = 8
        Width = 107
        Height = 15
        Caption = 'Non-routed delivery'
      end
      object lblNrdInfilLocation: TLabel
        Left = 173
        Top = 37
        Width = 211
        Height = 15
        Caption = 'Non-routed delivery infiltration location'
      end
      object lblSemi_Routed_Delivery: TLabel
        Left = 173
        Top = 66
        Width = 110
        Height = 15
        Caption = 'Semi-routed delivery'
      end
      object lblSemi_Routed_DeliveryLimits: TLabel
        Left = 173
        Top = 95
        Width = 142
        Height = 15
        Caption = 'Semi-routed delivery limits'
      end
      object lblSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE: TLabel
        Left = 173
        Top = 124
        Width = 203
        Height = 15
        Caption = 'Semi-routed delivery closure tolerance'
      end
      object lblReturnFlow: TLabel
        Left = 173
        Top = 152
        Width = 131
        Height = 15
        Caption = 'Return flow specification'
      end
      object comboNon_Routed_Delivery: TComboBox
        Left = 13
        Top = 5
        Width = 145
        Height = 23
        Style = csDropDownList
        Enabled = False
        ItemIndex = 0
        TabOrder = 0
        Text = 'Don'#39't use'
        Items.Strings = (
          'Don'#39't use'
          'Static'
          'Transient')
      end
      object comboNrdInfilLocation: TComboBox
        Left = 13
        Top = 34
        Width = 145
        Height = 23
        Style = csDropDownList
        Enabled = False
        ItemIndex = 0
        TabOrder = 1
        Text = 'Don'#39't use'
        Items.Strings = (
          'Don'#39't use'
          'Static'
          'Transient')
      end
      object comboSemi_Routed_Delivery: TComboBox
        Left = 13
        Top = 63
        Width = 145
        Height = 23
        Style = csDropDownList
        Enabled = False
        ItemIndex = 0
        TabOrder = 2
        Text = 'Don'#39't use'
        Items.Strings = (
          'Don'#39't use'
          'Static'
          'Transient')
      end
      object comboSemi_Routed_DeliveryLimits: TComboBox
        Left = 13
        Top = 92
        Width = 145
        Height = 23
        Style = csDropDownList
        Enabled = False
        ItemIndex = 0
        TabOrder = 3
        Text = 'Don'#39't use'
        Items.Strings = (
          'Don'#39't use'
          'Static'
          'Transient')
      end
      object rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE: TRbwDataEntry
        Left = 13
        Top = 121
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboReturnFlow: TComboBox
        Left = 13
        Top = 149
        Width = 145
        Height = 23
        Style = csDropDownList
        Enabled = False
        ItemIndex = 0
        TabOrder = 5
        Text = 'Don'#39't use'
        Items.Strings = (
          'Don'#39't use'
          'Static'
          'Transient')
      end
      object cbRebuild_Fully_Routed_Return: TCheckBox
        Left = 13
        Top = 178
        Width = 267
        Height = 17
        Caption = 'Rebuild fully routed return'
        Enabled = False
        TabOrder = 6
      end
    end
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = clbPrint
      end
      item
        Control = comboNon_Routed_Delivery
      end
      item
        Control = comboNrdInfilLocation
      end
      item
        Control = comboSemi_Routed_Delivery
      end
      item
        Control = comboSemi_Routed_DeliveryLimits
      end
      item
        Control = rdeSEMI_ROUTED_DELIVERY_CLOSURE_TOLERANCE
      end
      item
        Control = comboReturnFlow
      end
      item
        Control = cbRebuild_Fully_Routed_Return
      end>
  end
end
