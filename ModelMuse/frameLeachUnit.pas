unit frameLeachUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameFormulaGridUnit, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4, Vcl.Mask, JvExMask, JvSpin, Vcl.Buttons;

type
  TLeachColumns = (lcStartTime, lcEndTime, lcLeachChoice, lcFormula);

  TframeLeach = class(TframeFormulaGrid)
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect:
        Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frameLeach: TframeLeach;

implementation

{$R *.dfm}

procedure TframeLeach.GridSelectCell(Sender: TObject; ACol, ARow: Integer; var
    CanSelect: Boolean);
begin
  inherited;
  if (ARow >= Grid.FixedRows) and (ACol = Ord(lcFormula)) then
  begin
    CanSelect := Grid.ItemIndex[Ord(lcLeachChoice), ARow] in [0,3]
  end;
end;

end.
