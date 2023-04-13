{@abstract(The main purpose of @name is to define @link(TUndoImportDXFFile)
  which is used to import DXF files into GoPhast. It also defines
  @link(TUndoImportDXFFile) which can be used to undo the import.)}
unit frmImportDXFUnit;


interface

uses System.UITypes,
  Windows, SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls,
  ScreenObjectUnit, DXF_Structs, DXF_Utils,
  frmImportShapefileUnit, frmCustomImportSimpleFileUnit;

type
  {@abstract(@name is the command used to import
    DXF files or reverse the import.)}
  TUndoImportDXFFile = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  {@abstract(@name is used to import DXF files into GoPhast.)
    See @link(TfrmGoPhast.miImportDXFFileClick).}
  TfrmImportDXF = class(TfrmCustomImportSimpleFile)
    cbSingleObject: TCheckBox;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name frees @link(FDxfObject).
    procedure FormDestroy(Sender: TObject); override;
    procedure cbEnclosedCellsClick(Sender: TObject);
  private
    // @name: DXF_Object;
    // @name represents the contents of the DXF file.
    // DXF_Object is defined in DXF_Structs.
    FDxfObject: DXF_Object;
    FDxfName: string;
    // @name is used to transform the coordinates of P
    // based on OCS.
    function CoordConvert(P: Point3D; OCS: pMatrix): Point3D;
    // @name converts the entities from @link(FDxfObject) to
    // @link(TScreenObject)s.
    procedure SetData;
    // @name is used to update @link(frmProgressMM).
    procedure Think(const Sender: TObject; Message: string);
    { Private declarations }
  public
    // @name is used to open and read a DXF file.  It returns @true
    // if it was able to do so and the file contains something it can use.
    function GetData: boolean;
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, GoPhastTypes, DataSetUnit,
  RbwParser, UndoItems, frmProgressUnit, ModelMuseUtilities, FastGEO,
  ValueArrayStorageUnit, GIS_Functions, DataSetNamesUnit;

resourcestring
  StrImportDXFFile = 'import DXF file';
  StrTheDxfFileS = 'The ".dxf" file "%s" does not exist.';
  StrProgress = 'Progress';
  StrImportedFromDXFFi = 'Imported from DXF files';
  StrObject = 'Object ';
  StrDObjectsWereInva = '%d objects were invalid because they cross themselv' +
  'es and have been skipped.';
  StrThereWasAnErrorR = 'There was an error reading %s. If it is open by som' +
  'e other program, try closing that other program.';

{$R *.dfm}

procedure TfrmImportDXF.Think(const Sender: TObject; Message: string);
begin
  frmProgressMM.ProgressLabelCaption := Message;
end;

function TfrmImportDXF.GetData: boolean;
begin
  inherited;
  UpdateEvalAt;

  result := OpenDialogFile.Execute;
  if result then
  begin
    FDxfName := OpenDialogFile.FileName;
    if not FileExists(FDxfName) then
    begin
      result := False;
      Beep;
      MessageDlg(Format(StrTheDxfFileS, [FDxfName]),
        mtError, [mbOK], 0);
      Exit;
    end;
    Caption := Caption + ' - ' + FDxfName;
    GetDataSets;
    frmProgressMM.PopupParent := self;
    frmProgressMM.Caption := StrProgress;
    frmProgressMM.Show;
    try
      try
        FDxfObject := DXF_Object.Create(name);
        FDxfObject.Progress := frmProgressMM.pbProgress;
        FDxfObject.OnThinking := Think;
        FDxfObject.ReadFile(FDxfName, frmProgressMM.memoMessages.Lines);
      except on EInOutError do
        begin
          result := False;
          Beep;
          MessageDlg(Format(StrThereWasAnErrorR, [FDxfName]),
            mtError, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      frmProgressMM.Hide;
    end;

    result := FDxfObject.layer_lists.Count > 0;
    comboDataSets.ItemIndex := 0;
    comboInterpolators.ItemIndex := 4;
  end;
end;

procedure TfrmImportDXF.cbEnclosedCellsClick(Sender: TObject);
begin
  inherited;
  if cbEnclosedCells.Checked then
  begin
    cbSingleObject.Checked := False;
  end;
  cbSingleObject.Enabled := not cbEnclosedCells.Checked;
end;

function TfrmImportDXF.CoordConvert(P: Point3D; OCS: pMatrix): Point3D;
begin
  if OCS = nil then
  begin
    result := P;
  end
  else
  begin
    result := TransformPoint(OCS^, P);
  end;
end;

procedure TfrmImportDXF.SetData;
var
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  ScreenObjectList: TList;
  PointIndex: integer;
  DataArrayName: string;
  Position: integer;
  DataArray: TDataArray;
  EntityCount: Integer;
  LayerIndex: integer;
  ALayer: DXF_Layer;
  EntityListIndex: integer;
  EList: Entity_List;
  EntityIndex: integer;
  Entity: DXF_Entity;
  PrimitiveList: TPrimitiveList;
  PrimitiveIndex: integer;
  Points: pointlist;
  InvalidPointCount: integer;
  Undo: TUndoImportDXFFile;
  Root: string;
  ExistingObjectCount: integer;
  NewDataSets: TList;
  ValueArrayItem: TValueArrayItem;
  function ConvertPoint(const DXF_Point: Point3D): TPoint2D;
  begin
    result.X := DXF_Point.X;
    result.Y := DXF_Point.Y;
  end;
begin
  SetLength(Points, 0);
  InvalidPointCount := 0;
  AScreenObject := nil;
  frmGoPhast.PhastModel.BeginScreenObjectUpdate;
  frmGoPhast.CanDraw := False;
  try
    try
      NewDataSets := TList.Create;
      try
        MakeNewDataSet(NewDataSets, '_DXF_Z',
          strDefaultClassification + '|' + StrImportedFromDXFFi,
          comboDataSets.ItemIndex = 0);
        EntityCount := 0;
        for LayerIndex := 0 to FDxfObject.layer_lists.Count - 1 do
        begin
          ALayer := FDxfObject.layer_lists[LayerIndex];
          for EntityListIndex := 0 to ALayer.entity_lists.Count - 1 do
          begin
            if ALayer.entity_names[EntityListIndex] <> Block_.ClassName then
            begin
              EList := ALayer.entity_lists[EntityListIndex];
              EntityCount := EntityCount + EList.entities.Count;
            end;
          end;
        end;
        frmProgressMM.Caption := '';
        frmProgressMM.Prefix := StrObject;
        frmProgressMM.PopupParent := self;
        frmProgressMM.Show;
        frmProgressMM.pbProgress.Max := EntityCount;
        frmProgressMM.pbProgress.Position := 0;
        frmProgressMM.ProgressLabelCaption :=
          Format('0 out of %d.', [EntityCount]);
        DataArrayName := comboDataSets.Text;

        DataArray := frmGoPhast.PhastModel.DataArrayManager.
          GetDataSetByName(DataArrayName);
        Assert(DataArray <> nil);
        ScreenObjectList := TList.Create;
        //MultipleParts := false;
        try
          Undo := TUndoImportDXFFile.Create;
          try
            Root := TScreenObject.ValidName(
              ExtractFileRoot(OpenDialogFile.FileName)+ '_');
            if cbSingleObject.Checked
             then
            begin
              ScreenObjectList.Capacity := 1;
            end
            else
            begin
              ScreenObjectList.Capacity := EntityCount;
            end;
            ExistingObjectCount :=
              frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);
            ValueArrayItem := nil;
            for LayerIndex := 0 to FDxfObject.layer_lists.Count - 1 do
            begin
              ALayer := FDxfObject.layer_lists[LayerIndex];
              for EntityListIndex := 0 to ALayer.entity_lists.Count - 1 do
              begin
                if ALayer.entity_names[EntityListIndex] <> Block_.ClassName then
                begin
                  EList := ALayer.entity_lists[EntityListIndex];
                  for EntityIndex := 0 to EList.entities.Count - 1 do
                  begin
                    Entity := EList.entities[EntityIndex];
                    SetLength(PrimitiveList, 0);
                    Entity.GetCoordinates(PrimitiveList, CoordConvert, nil);
                    for PrimitiveIndex := 0 to Length(PrimitiveList) - 1 do
                    begin
                      Points := PrimitiveList[PrimitiveIndex];
                      if Length(Points) > 0 then
                      begin
                        {$REGION 'ObjectCreate'}
                        if (ScreenObjectList.Count = 0) or not cbSingleObject.Checked then
                        begin
                          AScreenObject :=
                            TScreenObject.CreateWithViewDirection(
                            frmGoPhast.PhastModel, vdTop,
                            UndoCreateScreenObject, False);
                          AScreenObject.Comment := 'Imported from ' + FDxfName +' on ' + DateTimeToStr(Now);
                          Inc(ExistingObjectCount);
                          AScreenObject.Name := Root + IntToStr(ExistingObjectCount);
                          AScreenObject.SetValuesOfEnclosedCells
                            := cbEnclosedCells.Checked;
                          AScreenObject.SetValuesOfIntersectedCells
                            := cbIntersectedCells.Checked;
                          AScreenObject.SetValuesByInterpolation
                            := cbInterpolation.Checked;
                          AScreenObject.ColorLine := Entity.colour <> clBlack;
                          AScreenObject.LineColor := Entity.colour;
                          AScreenObject.FillScreenObject := AScreenObject.ColorLine;
                          AScreenObject.FillColor := Entity.colour;
                          AScreenObject.ElevationCount := ecZero;
                          AScreenObject.Capacity := Length(Points);
                          AScreenObject.EvaluatedAt :=
                            TEvaluatedAt(rgEvaluatedAt.ItemIndex);
                          ScreenObjectList.Add(AScreenObject);
                          Position := AScreenObject.AddDataSet(DataArray);
                          if cbSingleObject.Checked then
                          begin
                            ValueArrayItem := AScreenObject.ImportedValues.Add;
                            ValueArrayItem.Name := 'Imported_' + DataArray.Name;
                            ValueArrayItem.Values.DataType := rdtDouble;
    //                          ValueArrayItem.Values.Count := EntityCount;
                            AScreenObject.DataSetFormulas[Position]
                              := rsObjectImportedValuesR + '("' + ValueArrayItem.Name + '")';
                          end;
                        end
                        else
                        begin
                          Position := AScreenObject.AddDataSet(DataArray);
                        end;
                        {$ENDREGION}
                        try
                          for PointIndex := 0 to Length(Points) - 1 do
                          begin
                            AScreenObject.AddPoint(ConvertPoint(Points[PointIndex]),
                              PointIndex = 0);
                          end;
                          Assert(Position >= 0);
                          if cbSingleObject.Checked then
                          begin
                            ValueArrayItem.Values.Add(Entity.p1.z);
                          end
                          else
                          begin
                            AScreenObject.DataSetFormulas[Position]
                              := FortranFloatToStr(Entity.p1.z);
                          end;
                        except on E: EScreenObjectError do
                          begin
                            Inc(InvalidPointCount);
                            Assert(ScreenObjectList.Last = AScreenObject);
                            ScreenObjectList.Delete(ScreenObjectList.Count-1);
                            AScreenObject.Free;
                            if cbSingleObject.Checked then
                            begin
                              raise;
                            end;
                          end
                        end;
                      end;
                    end;
                    frmProgressMM.StepIt;
                    Application.ProcessMessages;
                  end;
                end;
              end;
            end;
            if ScreenObjectList.Count > 0 then
            begin
              Undo.StoreNewScreenObjects(ScreenObjectList);
              Undo.StoreNewDataSets(NewDataSets);
              frmGoPhast.UndoStack.Submit(Undo);
              frmGoPhast.PhastModel.AddFileToArchive(FDxfName);
            end
            else
            begin
              Undo.Free
            end;
          except
            Undo.Free;
            raise;
          end;
        finally
          ScreenObjectList.Free;
          frmProgressMM.Hide;
        end;
      finally
        NewDataSets.Free;
      end;
    finally
      frmGoPhast.CanDraw := True;
      frmGoPhast.PhastModel.EndScreenObjectUpdate;
    end;
    if InvalidPointCount > 0 then
    begin
      Beep;
      MessageDlg(Format(StrDObjectsWereInva, [InvalidPointCount]),
        mtWarning, [mbOK], 0);
    end;
  except on E: EScreenObjectError do
    begin
      Beep;
      MessageDlg(E.message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmImportDXF.btnOKClick(Sender: TObject);
begin
  inherited;
  Hide;
  SetData;
end;

procedure TfrmImportDXF.FormDestroy(Sender: TObject);
begin
  inherited;
  FDxfObject.Free;
end;

{ TUndoImportDXFFile }

function TUndoImportDXFFile.Description: string;
begin
  result := StrImportDXFFile;
end;

end.


