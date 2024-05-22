inherited frameScreenObjectCfpFixed: TframeScreenObjectCfpFixed
  Width = 563
  Height = 447
  ExplicitWidth = 563
  ExplicitHeight = 447
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 563
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pcCfp: TPageControl
    Left = 0
    Top = 17
    Width = 563
    Height = 430
    ActivePage = tabSteady
    Align = alClient
    TabOrder = 1
    object tabSteady: TTabSheet
      Caption = 'Steady'
      DesignSize = (
        555
        400)
      object lblValue2: TLabel
        Left = 3
        Top = 288
        Width = 97
        Height = 15
        Caption = 'Limited flow value'
      end
      object lblValue3: TLabel
        Left = 3
        Top = 344
        Width = 116
        Height = 15
        Caption = 'Cauchy limited inflow'
      end
      object lblHint: TLabel
        Left = 159
        Top = 104
        Width = 310
        Height = 105
        Caption = 
          'CFP boundary condition values (N_HEAD) must be greater than or e' +
          'qual to -1. It is only necessary to specify CFP boundary conditi' +
          'on values for nodes that are CFP boundary condition nodes. Other' +
          ' nodes will automatically have their boundary condition N_HEAD v' +
          'alues set to -1 which is used to indicate that the head in the n' +
          'ode should be calculated.'
        WordWrap = True
      end
      object edValue2: TEdit
        Left = 3
        Top = 309
        Width = 442
        Height = 23
        TabOrder = 0
        OnChange = edValue2Change
      end
      object btnValue2: TButton
        Left = 457
        Top = 308
        Width = 90
        Height = 25
        Caption = 'Edit F()...'
        TabOrder = 1
      end
      object edValue3: TEdit
        Left = 0
        Top = 365
        Width = 442
        Height = 23
        TabOrder = 2
        OnChange = edValue3Change
      end
      object btnValue3: TButton
        Left = 457
        Top = 364
        Width = 90
        Height = 25
        Caption = 'Edit F()...'
        TabOrder = 3
      end
      object btnFixedHead: TButton
        Left = 386
        Top = 68
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        TabOrder = 4
      end
      object edFixedHead: TLabeledEdit
        Left = 159
        Top = 71
        Width = 215
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 57
        EditLabel.Height = 15
        EditLabel.Caption = 'Fixed head'
        TabOrder = 5
        Text = ''
        OnChange = edFixedHeadChange
      end
      object cbTimeDependent: TCheckBox
        Left = 3
        Top = 23
        Width = 185
        Height = 17
        Caption = 'Time Dependent'
        TabOrder = 6
        OnClick = cbTimeDependentClick
      end
      object rgBoundaryType: TRadioGroup
        Left = 3
        Top = 55
        Width = 150
        Height = 113
        Caption = 'Boundary type'
        ItemIndex = 0
        Items.Strings = (
          'Defined head'
          'Well'
          'Robin (Cauchy)'
          'Limited Head')
        TabOrder = 7
        OnClick = rgBoundaryTypeClick
      end
    end
    object tabTransient: TTabSheet
      Caption = 'Transient'
      ImageIndex = 1
      inline frameTimeDependent: TframeGrid
        Left = 0
        Top = 0
        Width = 555
        Height = 400
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 555
        ExplicitHeight = 400
        inherited Panel: TPanel
          Top = 359
          Width = 555
          ExplicitTop = 354
          ExplicitWidth = 555
          inherited sbAdd: TSpeedButton
            Left = 290
            ExplicitLeft = 290
          end
          inherited sbInsert: TSpeedButton
            Left = 343
            ExplicitLeft = 343
          end
          inherited sbDelete: TSpeedButton
            Left = 396
            ExplicitLeft = 396
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 555
          Height = 359
          OnSetEditText = frameTimeDependentGridSetEditText
          OnEndUpdate = frameTimeDependentGridEndUpdate
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = True
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
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 555
          ExplicitHeight = 359
        end
      end
    end
  end
end
