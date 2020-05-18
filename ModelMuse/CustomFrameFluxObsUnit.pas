// @name defines a frame that is used to define the flux observation data
// for a particular @link(TScreenObject).
unit CustomFrameFluxObsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, RbwDataGrid4, StdCtrls, FluxObservationUnit;

type
  // @name is a frame that is used to define the flux observation data
  // for a particular @link(TScreenObject).
  TCustomframeFluxObs = class(TFrame)
    // @name holds the flux observation data.
    // The checkbox in column 1 indicates whether the @link(TScreenObject)
    // being edited is part of the indicated flux observation.
    // The text is column 2 is the Factor formula.
    rdgObservationGroups: TRbwDataGrid4;
    // @name is used to indicate the type of flux observation.
    lblFluxObservations: TLabel;
    btnAddOrRemoveFluxObservations: TButton;
  private
    { Private declarations }
  public
    {@name sets the captions in @link(rdgObservationGroups).}
    procedure InitializeControls;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Math, ScreenObjectUnit, frmCustomGoPhastUnit;

{ TCustomframeFluxObs }

procedure TCustomframeFluxObs.InitializeControls;
begin
  inherited;
  rdgObservationGroups.BeginUpdate;
  try
    ClearGrid(rdgObservationGroups);
    rdgObservationGroups.Cells[0,0] := 'N';
    rdgObservationGroups.Cells[1,0] := 'Observation group';
    rdgObservationGroups.Cells[2,0] := 'Factor';
    rdgObservationGroups.ColWidths[2] := 200;
  finally
    rdgObservationGroups.EndUpdate
  end;
end;

end.
