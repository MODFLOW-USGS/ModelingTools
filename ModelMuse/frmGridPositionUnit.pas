unit frmGridPositionUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  ArgusDataEntry, Vcl.Buttons;

type
  TfrmGridPosition = class(TfrmCustomGoPhast)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    lblGridOriginX: TLabel;
    rdeGridOriginX: TRbwDataEntry;
    lblGridOriginY: TLabel;
    rdeGridOriginY: TRbwDataEntry;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmGridPosition: TfrmGridPosition;

implementation

uses
  GoPhastTypes, FastGEO, frmGoPhastUnit, UndoItems;

{$R *.dfm}

{ TfrmGridPosition }

procedure TfrmGridPosition.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmGridPosition.FormCreate(Sender: TObject);
begin
  inherited;
  GetData
end;

procedure TfrmGridPosition.GetData;
var
  RowPositions: TOneDRealArray;
  ColumnPositions: TOneDRealArray;
  GridOrigin: TPoint2D;
begin
  RowPositions := frmGoPhast.Grid.RowPositions;
  ColumnPositions := frmGoPhast.Grid.ColumnPositions;

  if Length(ColumnPositions) > 0 then
  begin
    GridOrigin.x := ColumnPositions[0];
  end
  else
  begin
    GridOrigin.x := 0;
  end;
  if Length(RowPositions) > 0 then
  begin
    GridOrigin.y := RowPositions[0];
  end
  else
  begin
    GridOrigin.y := 0;
  end;
  GridOrigin := frmGoPhast.Grid.RotateFromGridCoordinatesToRealWorldCoordinates(GridOrigin);
  rdeGridOriginX.Text := FloatToStr(GridOrigin.x);
  rdeGridOriginY.Text := FloatToStr(GridOrigin.Y);
end;

procedure TfrmGridPosition.SetData;
var
  GridOrigin: TPoint2D;
begin
  GridOrigin.x := rdeGridOriginX.RealValue;
  GridOrigin.Y := rdeGridOriginY.RealValue;
  frmGoPhast.UndoStack.Submit(TUndoMoveGrid.Create(GridOrigin));
end;

end.
