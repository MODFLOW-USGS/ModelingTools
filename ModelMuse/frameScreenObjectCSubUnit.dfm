inherited frameScreenObjectCSub: TframeScreenObjectCSub
  inherited pcMain: TPageControl
    ExplicitWidth = 396
    ExplicitHeight = 332
    inherited tabTransient: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 388
      ExplicitHeight = 304
      inherited pnlBottom: TPanel
        ExplicitTop = 258
        ExplicitWidth = 388
        inherited btnDelete: TBitBtn
          ExplicitLeft = 300
        end
        inherited btnInsert: TBitBtn
          ExplicitLeft = 212
        end
      end
      inherited pnlGrid: TPanel
        ExplicitWidth = 388
        ExplicitHeight = 258
        inherited pnlEditGrid: TPanel
          ExplicitWidth = 386
        end
        inherited rdgModflowBoundary: TRbwDataGrid4
          OnMouseUp = nil
          OnSelectCell = nil
          OnSetEditText = nil
          OnStateChange = nil
          OnHorizontalScroll = nil
          ExplicitWidth = 386
        end
      end
    end
  end
end
