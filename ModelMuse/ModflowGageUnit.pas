unit ModflowGageUnit;

interface

uses Classes, GoPhastTypes, ModflowBoundaryUnit, SysUtils;

Type
  TStreamGage = class(TGoPhastPersistent)
  private
    FGage2: boolean;
    FGage3: boolean;
    FGage0: boolean;
    FGage1: boolean;
    FGage6: boolean;
    FGage7: boolean;
    FGage5: boolean;
    FScreenObject: TObject;
    procedure SetGage0(const Value: boolean);
    procedure SetGage1(const Value: boolean);
    procedure SetGage2(const Value: boolean);
    procedure SetGage3(const Value: boolean);
    procedure SetGage5(const Value: boolean);
    procedure SetGage6(const Value: boolean);
    procedure SetGage7(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    function Used: boolean;
    Constructor Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure Evaluate(DataSets: TList; AModel: TBaseModel);
  published
    property Gage0: boolean read FGage0 write SetGage0;
    property Gage1: boolean read FGage1 write SetGage1;
    property Gage2: boolean read FGage2 write SetGage2;
    property Gage3: boolean read FGage3 write SetGage3;
    property Gage5: boolean read FGage5 write SetGage5;
    property Gage6: boolean read FGage6 write SetGage6;
    property Gage7: boolean read FGage7 write SetGage7;
  end;

implementation

uses
  RbwParser, ScreenObjectUnit, DataSetUnit, PhastModelUnit;

resourcestring
  StrAssignedByS = 'Assigned by %s';

{ TStreamGage }

procedure TStreamGage.Assign(Source: TPersistent);
var
  SourceGage: TStreamGage;
begin
  if Source is TStreamGage then
  begin
    SourceGage := TStreamGage(Source);
    Gage0 := SourceGage.Gage0;
    Gage1 := SourceGage.Gage1;
    Gage2 := SourceGage.Gage2;
    Gage3 := SourceGage.Gage3;
    Gage5 := SourceGage.Gage5;
    Gage6 := SourceGage.Gage6;
    Gage7 := SourceGage.Gage7;
  end
  else
  begin
    inherited;
  end;
end;

constructor TStreamGage.Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
begin
  inherited Create(InvalidateModelEvent);
  FScreenObject := ScreenObject;
end;

procedure TStreamGage.Evaluate(DataSets: TList; AModel: TBaseModel);
var
  CellList: TCellAssignmentList;
  DataArray: TDataArray;
  ScreenObject: TScreenObject;
  Annotation: string;
  Index: Integer;
  Cell: TCellAssignment;
  LocalModel: TCustomModel;
  procedure AssignValues;
  var
    Index: Integer;
    Cell: TCellAssignment;
  begin
    Assert(DataArray.DataType = rdtBoolean);
    for Index := 0 to CellList.Count - 1 do
    begin
      Cell := CellList[Index];
      if Cell.LgrEdge then
      begin
        Continue;
      end;
      DataArray.BooleanData[Cell.Layer, Cell.Row, Cell.Column] := True;
      DataArray.Annotation[Cell.Layer, Cell.Row, Cell.Column] := Annotation;
    end;
  end;
begin
  Assert(DataSets.Count = 7);
  CellList := TCellAssignmentList.Create;
  try
    ScreenObject := (FScreenObject as TScreenObject);
    ScreenObject.GetModpathCellList(CellList, AModel);
    LocalModel := AModel as TCustomModel;
    if CellList.Count > 0 then
    begin
      for Index := 0 to CellList.Count - 1 do
      begin
        Cell := CellList[Index];
        if Cell.LgrEdge then
        begin
          Continue;
        end;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Annotation := Format(StrAssignedByS, [ScreenObject.Name]);
      if Gage0 then
      begin
        DataArray := DataSets[0];
        AssignValues;
      end;
      if Gage1 then
      begin
        DataArray := DataSets[1];
        AssignValues;
      end;
      if Gage2 then
      begin
        DataArray := DataSets[2];
        AssignValues;
      end;
      if Gage3 then
      begin
        DataArray := DataSets[3];
        AssignValues;
      end;
      if Gage5 then
      begin
        DataArray := DataSets[4];
        AssignValues;
      end;
      if Gage6 then
      begin
        DataArray := DataSets[5];
        AssignValues;
      end;
      if Gage7 then
      begin
        DataArray := DataSets[6];
        AssignValues;
      end;
    end;
  finally
    CellList.Free;
  end;
end;

procedure TStreamGage.SetGage0(const Value: boolean);
begin
  if FGage0 <> Value then
  begin
    InvalidateModel;
    FGage0 := Value;
  end;
end;

procedure TStreamGage.SetGage1(const Value: boolean);
begin
  if FGage1 <> Value then
  begin
    InvalidateModel;
    FGage1 := Value;
  end;
end;

procedure TStreamGage.SetGage2(const Value: boolean);
begin
  if FGage2 <> Value then
  begin
    InvalidateModel;
    FGage2 := Value;
  end;
end;

procedure TStreamGage.SetGage3(const Value: boolean);
begin
  if FGage3 <> Value then
  begin
    InvalidateModel;
    FGage3 := Value;
  end;
end;

procedure TStreamGage.SetGage5(const Value: boolean);
begin
  if FGage5 <> Value then
  begin
    InvalidateModel;
    FGage5 := Value;
  end;
end;

procedure TStreamGage.SetGage6(const Value: boolean);
begin
  if FGage6 <> Value then
  begin
    InvalidateModel;
    FGage6 := Value;
  end;
end;

procedure TStreamGage.SetGage7(const Value: boolean);
begin
  if FGage7 <> Value then
  begin
    InvalidateModel;
    FGage7 := Value;
  end;
end;

function TStreamGage.Used: boolean;
begin
  result := Gage0 or Gage1 or Gage2 or Gage3 or Gage5 or Gage6 or Gage7;
end;

end.
