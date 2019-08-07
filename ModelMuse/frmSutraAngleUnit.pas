unit frmSutraAngleUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Mask, JvExMask, JvSpin, Buttons, ExtCtrls, Grids, RbwDataGrid4;

type
  TPointGridColumns = (pgcNone, pgcX, pgcY);
  TPointGridRows = (pgrNone, pgrStart, pgrEnd);

  TfrmSutraAngle = class(TfrmCustomGoPhast)
    seAngle: TJvSpinEdit;
    lblAngle: TLabel;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    rgSpecify: TRadioGroup;
    rdgEndPoints: TRbwDataGrid4;
    procedure seAngleButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure seAngleChange(Sender: TObject);
    procedure rgSpecifyClick(Sender: TObject);
    procedure rdgEndPointsEndUpdate(Sender: TObject);
  private
    FChanged: Boolean;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSutraAngle: TfrmSutraAngle;

implementation

uses
  frmGoPhastUnit, UndoItems, InteractiveTools, FastGEO, SutraMeshUnit,
  DrawMeshTypesUnit;

resourcestring
  StrX = 'X';
  StrY = 'Y';
  StrStart = 'Start';
  StrEnd = 'End';

{$R *.dfm}

{ TfrmSutraAngle }

procedure TfrmSutraAngle.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSutraAngle.FormCreate(Sender: TObject);
begin
  inherited;

  rdgEndPoints.BeginUpdate;
  try
    rgSpecifyClick(nil);
    rdgEndPoints.Cells[Ord(pgcX), Ord(pgrNone)] := StrX;
    rdgEndPoints.Cells[Ord(pgcY), Ord(pgrNone)] := StrY;
    rdgEndPoints.Cells[Ord(pgcNone), Ord(pgrStart)] := StrStart;
    rdgEndPoints.Cells[Ord(pgcNone), Ord(pgrEnd)] := StrEnd;
    GetData;
  finally
    rdgEndPoints.EndUpdate;
  end;
end;

procedure TfrmSutraAngle.GetData;
var
  Value: Double;
  CrossSection: TMeshCrossSectionLine;
begin
  CrossSection := frmGoPhast.PhastModel.DrawMesh.CrossSection;
  Value := CrossSection.Angle;
  while Value > Pi/2 do
  begin
    Value := Value-Pi;
  end;
  while Value < -Pi/2 do
  begin
    Value := Value+Pi;
  end;
  seAngle.Value := Value * 180/Pi;

  rdgEndPoints.Cells[Ord(pgcX), Ord(pgrStart)] :=
    FloatToStr(CrossSection.StartX);
  rdgEndPoints.Cells[Ord(pgcY), Ord(pgrStart)] :=
    FloatToStr(CrossSection.StartY);
  rdgEndPoints.Cells[Ord(pgcX), Ord(pgrEnd)] :=
    FloatToStr(CrossSection.EndX);
  rdgEndPoints.Cells[Ord(pgcY), Ord(pgrEnd)] :=
    FloatToStr(CrossSection.EndY);

  FChanged := False;
end;

procedure TfrmSutraAngle.rdgEndPointsEndUpdate(Sender: TObject);
begin
  inherited;
  FChanged := True;
end;

procedure TfrmSutraAngle.rgSpecifyClick(Sender: TObject);
begin
  inherited;
  seAngle.Enabled := rgSpecify.ItemIndex = 0;
  rdgEndPoints.Enabled := rgSpecify.ItemIndex = 1;
  if rdgEndPoints.Enabled then
  begin
    rdgEndPoints.Color := clWindow;
  end
  else
  begin
    rdgEndPoints.Color := clBtnFace;
  end;
end;

procedure TfrmSutraAngle.seAngleButtonClick(Sender: TObject);
begin
  inherited;
  seAngle.Value := Round(seAngle.Value);
end;

procedure TfrmSutraAngle.seAngleChange(Sender: TObject);
begin
  inherited;
  FChanged := True;
end;

procedure TfrmSutraAngle.SetData;
var
  NewAngle: Extended;
  NewLocation: TSegment2D;
  Undo: TUndoSpecifyCrossSection;
begin
  if FChanged then
  begin
    case rgSpecify.ItemIndex of
      0:
        begin
          // Angle
          NewAngle := seAngle.Value * Pi / 180;
          SetNewCrossSectionAngle(NewAngle, NewLocation);
        end;
      1:
        begin
          NewLocation[1].x :=
            StrToFloat(rdgEndPoints.Cells[Ord(pgcX), Ord(pgrStart)]);
          NewLocation[1].y :=
            StrToFloat(rdgEndPoints.Cells[Ord(pgcY), Ord(pgrStart)]);
          NewLocation[2].x :=
            StrToFloat(rdgEndPoints.Cells[Ord(pgcX), Ord(pgrEnd)]);
          NewLocation[2].y :=
            StrToFloat(rdgEndPoints.Cells[Ord(pgcY), Ord(pgrEnd)]);
        end;
      else Assert(False);
    end;
    Undo := TUndoSpecifyCrossSection.Create(NewLocation);
    frmGoPhast.UndoStack.Submit(Undo);
  end;
end;

end.
