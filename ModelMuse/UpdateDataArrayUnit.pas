unit UpdateDataArrayUnit;

interface

uses
  Classes, DataSetUnit, GoPhastTypes, RbwParser;

type
  TUpdataDataArrayRecord = record
    Model: TBaseModel;
    OnDataSetUsed: TObjectUsedEvent;
    OldDataArrayName: string;
    NewName: string;
    NewDisplayName: string;
    NewFormula: string;
    AssociatedDataSets: string;
    ShouldCreate: Boolean;
    Classification: string;
    Orientation: TDataSetOrientation;
    DataType: TRbwDataType;
  end;

procedure UpdateOrCreateDataArray(const UpdataDat: TUpdataDataArrayRecord); overload;

implementation

uses
  PhastModelUnit, DataSetNamesUnit;

procedure UpdateOrCreateDataArray(const UpdataDat: TUpdataDataArrayRecord); overload;
var
  DataArray: TDataArray;
  LocalModel: TPhastModel;
begin
  Assert(UpdataDat.Model <> nil);
  LocalModel := UpdataDat.Model as TPhastModel;
  if not (csLoading in LocalModel.ComponentState) then
  begin
    DataArray := LocalModel.DataArrayManager.GetDataSetByName(UpdataDat.NewName);
    if DataArray = nil then
    begin
      DataArray := LocalModel.DataArrayManager.
        GetDataSetByName(UpdataDat.OldDataArrayName);
    end;
    if DataArray <> nil then
    begin
      if DataArray.Name <> UpdataDat.NewName then
      begin
        LocalModel.RenameDataArray(DataArray, UpdataDat.NewName, UpdataDat.NewDisplayName);
      end;
    end;
    if DataArray = nil then
    begin
      if UpdataDat.ShouldCreate then
      begin
        // create a new data array.
        DataArray := LocalModel.DataArrayManager.CreateNewDataArray(
          TDataArray, UpdataDat.NewName, UpdataDat.NewFormula, UpdataDat.NewDisplayName,
          [dcName, dcType, dcOrientation, dcEvaluatedAt],
          UpdataDat.DataType, eaBlocks, UpdataDat.Orientation,
          UpdataDat.Classification);
      end;
    end;
    if DataArray <> nil then
    begin
      if UpdataDat.Orientation = dso3D then
      begin
        LocalModel.ThreeDGridObserver.TalksTo(DataArray);
      end
      else if UpdataDat.Orientation = dsoTop then
      begin
        LocalModel.TopGridObserver.TalksTo(DataArray);
      end
      else
      begin
        Assert(False);
      end;
      DataArray.UpdateDimensions(LocalModel.LayerCount,
        LocalModel.RowCount, LocalModel.ColumnCount);
      DataArray.OnDataSetUsed := UpdataDat.OnDataSetUsed;
      DataArray.AssociatedDataSets := UpdataDat.AssociatedDataSets;
      DataArray.Classification := UpdataDat.Classification;
    end;
  end;
end;

end.
