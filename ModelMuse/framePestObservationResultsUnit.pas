unit framePestObservationResultsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomObservationResultsUnit,
  QuadTreeClass, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Grids, RbwDataGrid4, JvSpin,
  JvExControls, JvColorBox, JvColorButton, frameDisplayLimitUnit, Vcl.Mask,
  JvExMask, JvToolEdit, Vcl.ComCtrls;

type
  TframePestObservationResults = class(TframeCustomObservationResults)
  private
    { Private declarations }
  protected
    procedure UpdateObsLinkList; override;
  public
    { Public declarations }
  end;

var
  framePestObservationResults: TframePestObservationResults;

implementation

{$R *.dfm}

{ TframePestObservationResults }

procedure TframePestObservationResults.UpdateObsLinkList;
begin
  inherited;

end;

end.
