unit frameScreenObjectFhbFlowUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectFhbHeadUnit, Grids,
  RbwDataGrid4, StdCtrls, ArgusDataEntry, Buttons, Mask, JvExMask,
  JvSpin, ExtCtrls, ModflowFhbUnit, ScreenObjectUnit,
  UndoItemsScreenObjects;

type
  TframeScreenObjectFhbFlow = class(TframeScreenObjectFhbHead)
    comboFormulaInterp: TComboBox;
    lblConductanceInterpretation: TLabel;
    procedure comboFormulaInterpChange(Sender: TObject);
  private
    procedure AssignFlowCaption;
    { Private declarations }
  protected
    procedure InitializeControls; override;
    function GetBoundary(AScreenObject: TScreenObject): TFhbHeadBoundary; override;
    function CreateNewBoundary: TFhbHeadBoundary; override;
    procedure CreateScreenObjectBoundary(AScreenObject: TScreenObject); override;
    procedure DoChange; override;
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectFhbFlow: TframeScreenObjectFhbFlow;

implementation

uses
  ModflowBoundaryUnit;

resourcestring
  StrFlowRatePerUnitL = 'Flow rate per unit length or area';
  StrFlowRate = 'Flow rate';
  StrTotalFlowRatePer = 'Total flow rate (per layer)';

{$R *.dfm}

procedure TframeScreenObjectFhbFlow.comboFormulaInterpChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

function TframeScreenObjectFhbFlow.CreateNewBoundary: TFhbHeadBoundary;
begin
  result := TFhbFlowBoundary.Create(nil, nil);
end;

procedure TframeScreenObjectFhbFlow.CreateScreenObjectBoundary(
  AScreenObject: TScreenObject);
begin
  AScreenObject.CreateFhbFlowBoundary
end;

procedure TframeScreenObjectFhbFlow.DoChange;
begin
  inherited;
  AssignFlowCaption;
end;

function TframeScreenObjectFhbFlow.GetBoundary(
  AScreenObject: TScreenObject): TFhbHeadBoundary;
begin
  result := AScreenObject.ModflowFhbFlowBoundary;
end;

procedure TframeScreenObjectFhbFlow.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  ScreenObjectIndex: Integer;
begin
  inherited;
  if FInitialListOfScreenObjects.Count > 0 then
  begin
    comboFormulaInterp.ItemIndex := Ord(FInitialListOfScreenObjects[0].
      ModflowFhbFlowBoundary.FormulaInterpretation);
    for ScreenObjectIndex := 1 to FInitialListOfScreenObjects.Count - 1 do
    begin
      if comboFormulaInterp.ItemIndex <> Ord(FInitialListOfScreenObjects[ScreenObjectIndex].
      ModflowFhbFlowBoundary.FormulaInterpretation) then
      begin
        comboFormulaInterp.ItemIndex := -1;
        Exit;
      end;
    end;
  end;
end;

procedure TframeScreenObjectFhbFlow.InitializeControls;
begin
  inherited;
  comboFormulaInterp.ItemIndex := 0;
  AssignFlowCaption;
  LayoutMultiRowEditControls;
end;

procedure TframeScreenObjectFhbFlow.AssignFlowCaption;
begin
  case comboFormulaInterp.ItemIndex of
    0:
      begin
        rdgModflowBoundary.Cells[Ord(fhcHead), 0] := StrFlowRatePerUnitL;
      end;
    1:
      begin
        rdgModflowBoundary.Cells[Ord(fhcHead), 0] := StrFlowRate;
      end;
    2:
      begin
        rdgModflowBoundary.Cells[Ord(fhcHead), 0] := StrTotalFlowRatePer;
      end;
  end;
end;

procedure TframeScreenObjectFhbFlow.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  ScreenObjectIndex: Integer;
  FormulaInterp: TFormulaInterpretation;
  Boundary: TFhbFlowBoundary;
begin
  inherited;
  if ClearAll or (comboFormulaInterp.ItemIndex < 0) then
  begin
    Exit;
  end;
  FormulaInterp := TFormulaInterpretation(comboFormulaInterp.ItemIndex);
  for ScreenObjectIndex := 0 to List.Count - 1 do
  begin
    Boundary := List[ScreenObjectIndex].ScreenObject.ModflowFhbFlowBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      Boundary.FormulaInterpretation := FormulaInterp;
    end;
  end;
end;

end.
