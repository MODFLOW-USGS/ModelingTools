unit SutraInitialConditionsWriterUnit;

interface

uses System.UITypes,
  Windows, CustomModflowWriterUnit, SysUtils, DataSetUnit, PhastModelUnit,
  Generics.Collections, Classes, SutraOptionsUnit;

type
  TDataValue = class(TObject)
    Number: integer;
    Value: double;
  end;

  TDataValueList = TObjectList<TDataValue>;

  TSutraInitialConditionsWriter = class(TCustomFileWriter)
  private
    FRestartFile: TStreamReader;
    FOptions: TSutraOptions;
    FLimit: Integer;
    FFileName: string;
    procedure WriteDataArray(DataArray: TDataArray; const ID, Prefix: string);
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel); reintroduce;
    procedure WriteFile(FileName: string);
  end;

implementation

uses
  GoPhastTypes, SutraMeshUnit, Generics.Defaults,
  SutraFileWriterUnit, IOUtils, Dialogs, PlProcUnit;

resourcestring
  StrTheRestartFileS = 'The restart file "%s" does not exist.';
  StrTheEndOfTheResta = 'The end of the restart file, %0:s, was reached before' +
  ' reading all the data required for data set %1:d of the initial conditions f' +
  'ile.';



{ TSutraInitialConditionsWriter }

constructor TSutraInitialConditionsWriter.Create(AModel: TCustomModel);
begin
  inherited Create(AModel, etExport);
end;

class function TSutraInitialConditionsWriter.Extension: string;
begin
  Assert(False);
end;

procedure TSutraInitialConditionsWriter.WriteDataArray(DataArray: TDataArray;
  const ID, Prefix: string);
var
  List: TDataValueList;
  index: Integer;
  NodeIndex: Integer;
  Mesh2D: TSutraMesh2D;
  LayerIndex: Integer;
  Mesh3D: TSutraMesh3D;
  Node3D: TSutraNode3D;
  DataValue: TDataValue;
  ParamUsed: Boolean;
  TempFileName: string;
  ScriptWriter: TSutraInitCondScriptWriter;
  DataFileWriter: TSutraNodeDataWriter;
//  Mesh: TSutraMesh3D;
begin
  Assert(DataArray.EvaluatedAt = eaNodes);
  ParamUsed := Model.PestUsed and DataArray.PestParametersUsed;
  DataArray.Initialize;
  if (DataArray.IsUniform = iuTrue) and not ParamUsed then
  begin
    WriteString('''UNIFORM''');
    NewLine;
    WriteFloat(DataArray.UniformRealValue);
    NewLine;
  end
  else
  begin
    WriteString('''NONUNIFORM''');
    NewLine;
    List := TDataValueList.Create;
    try
      TempFileName := '';
      if ParamUsed then
      begin
        DataFileWriter := TSutraNodeDataWriter.Create(Model, etExport);
        try
          DataFileWriter.WriteFile(FFileName, DataArray);
        finally
          DataFileWriter.Free;
        end;


        TempFileName := ChangeFileExt(FFileName, '.' + ID);
        WriteString('@INSERT 99 ');
        WriteString(ExtractFileName(TempFileName));
        Model.FilesToDelete.Add(TempFileName);
        NewLine;
        ScriptWriter := TSutraInitCondScriptWriter.Create(Model, etExport);
        try
          ScriptWriter.WriteFiles(FFileName, DataArray.Name, ID, Prefix);
        finally
          ScriptWriter.Free;
        end;
//        Mesh := Model.SutraMesh;
      end;

      if ParamUsed then
      begin
        OpenTempfile(TempFileName)
      end;
      try
        begin
          case Model.SutraMesh.MeshType of
            mt2D, mtProfile:
              begin
                Mesh2D := Model.SutraMesh.Mesh2D;
                for NodeIndex := 0 to Mesh2D.Nodes.Count - 1 do
                begin
                  DataValue := TDataValue.Create;
                  List.Add(DataValue);
                  DataValue.Value := DataArray.RealData[0,0,NodeIndex];
                  DataValue.Number := Mesh2D.Nodes[NodeIndex].Number;
                end;
              end;
            mt3D:
              begin
                Mesh3D := Model.SutraMesh;
                Mesh2D := Mesh3D.Mesh2D;
                for LayerIndex := 0 to Mesh3D.LayerCount do
                begin
                  for NodeIndex := 0 to Mesh2D.Nodes.Count - 1 do
                  begin
                    Node3D := Mesh3D.NodeArray[LayerIndex, NodeIndex];
                    if Node3D.Active then
                    begin
                      DataValue := TDataValue.Create;
                      List.Add(DataValue);
                      DataValue.Value := DataArray.RealData[LayerIndex,0,NodeIndex];
                      DataValue.Number := Node3D.Number;
                    end;
                  end;
                end;
              end;
            else
              Assert(False);
          end;
          List.Sort(TComparer<TDataValue>.Construct(
            function (const L, R: TDataValue): integer
            begin
              result := L.Number - R.Number;
            end));
          for index := 0 to List.Count - 1 do
          begin
            DataValue := List[index];
            WriteFloat(DataValue.Value);
            if ((index + 1) mod 10) = 0 then
            begin
              NewLine;
            end;
          end;
          if (List.Count  mod 10) <> 0 then
          begin
            NewLine;
          end;
        end;
      finally
        if ParamUsed then
        begin
          CloseTempFile;
        end;
      end;
    finally
      List.Free;
    end;
  end;

  Model.DataArrayManager.AddDataSetToCache(DataArray);
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TSutraInitialConditionsWriter.WriteDataSet0;
begin
  WriteCommentLine(File_Comment('SUTRA initial conditions file'));
end;

procedure TSutraInitialConditionsWriter.WriteDataSet1;
var
  TICS: double;
begin
  WriteCommentLine('Data set 1');

  TICS := (Model as TPhastModel).SutraTimeOptions.InitialTime;
  WriteFloat(TICS);
  NewLine;
end;

procedure TSutraInitialConditionsWriter.WriteDataSet2;
var
  InitialPressure: TDataArray;
  index: Integer;
  ALine: string;
begin
  WriteCommentLine('Data set 2');

  if FOptions.ReadStart in [rsPressure, rsBoth] then
  begin
    Assert(Assigned(FRestartFile));
    ALine := FRestartFile.ReadLine;
    WriteString(ALine);
    NewLine;
    for index := 0 to FLimit - 1 do
    begin
      ALine := FRestartFile.ReadLine;
      WriteString(ALine);
      NewLine;
      if FRestartFile.EndOfStream then
      begin
        Beep;
        MessageDlg(Format(StrTheEndOfTheResta,
          [FOptions.FullReadStartRestartFileName, 2]),
          mtError, [mbOK], 0);
        break;
      end;
    end;
  end
  else
  begin
    InitialPressure := nil;
    case (Model as TPhastModel).SutraOptions.TransportChoice of
      tcSolute, tcEnergy, tcFreezing:
        InitialPressure := Model.DataArrayManager.GetDataSetByName(KInitialPressure);
      tcSoluteHead:
        InitialPressure := Model.DataArrayManager.GetDataSetByName(rsInitial_Head);
      else
        Assert(False);
    end;

    WriteDataArray(InitialPressure, 'PVEC', 'PVC');

  end;
end;

procedure TSutraInitialConditionsWriter.WriteDataSet3;
var
  InitialU: TDataArray;
  index: Integer;
  ALine: string;
begin
  WriteCommentLine('Data set 3');

  if FOptions.ReadStart in [rsU, rsBoth] then
  begin
    Assert(Assigned(FRestartFile));
    if FOptions.ReadStart = rsU then
    begin
      FRestartFile.ReadLine;
      for index := 0 to FLimit - 1 do
      begin
        FRestartFile.ReadLine;
      end;
    end;
    ALine := FRestartFile.ReadLine;
    WriteString(ALine);
    NewLine;
    for index := 0 to FLimit - 1 do
    begin
      ALine := FRestartFile.ReadLine;
      WriteString(ALine);
      NewLine;
      if FRestartFile.EndOfStream then
      begin
        Beep;
        MessageDlg(Format(StrTheEndOfTheResta,
          [FOptions.FullReadStartRestartFileName, 3]),
          mtError, [mbOK], 0);
        break;
      end;
    end;
  end
  else
  begin
    InitialU := nil;
    case Model.SutraOptions.TransportChoice of
      tcSolute, tcSoluteHead: InitialU := Model.DataArrayManager.GetDataSetByName(
        KInitialConcentration);
      tcEnergy, tcFreezing: InitialU := Model.DataArrayManager.GetDataSetByName(
        KInitialTemperature);
      else
        Assert(False);
    end;
    WriteDataArray(InitialU, 'UVEC', 'UVC');
  end;
end;

procedure TSutraInitialConditionsWriter.WriteFile(FileName: string);
var
  ErrorMessage: string;
  Mesh: TSutraMesh3D;
  NodeCount: Integer;
begin
  FOptions := Model.SutraOptions;
  if (FOptions.StartType = stWarm) and (FOptions.FullRestartFileName <> '') then
  begin
    SutraFileWriter.AddFile(sftIcs, FOptions.FullRestartFileName);
  end
  else
  begin
    FRestartFile := nil;
    try
      if FOptions.ReadStart in [rsPressure, rsU, rsBoth] then
      begin
         Model.AddModelInputFile(FOptions.FullReadStartRestartFileName);
         if not TFile.Exists(FOptions.FullReadStartRestartFileName) then
         begin
           Beep;
           ErrorMessage := Format(StrTheRestartFileS,
             [FOptions.FullReadStartRestartFileName]);
           MessageDlg(ErrorMessage, mtError, [mbOK], 0);
           Exit;
         end
         else
         begin
           FRestartFile := TFile.OpenText(FOptions.FullReadStartRestartFileName);
           FRestartFile.ReadLine;
           Mesh := Model.Mesh as TSutraMesh3D;
           if Mesh.MeshType = mt3D then
           begin
             NodeCount := Mesh.ActiveNodeCount;
           end
           else
           begin
             NodeCount := Mesh.Mesh2D.Nodes.Count;
           end;
           FLimit := (NodeCount - 1) div 4 + 1;
         end;
      end;


      FFileName := ChangeFileExt(FileName, '.ics');
      OpenFile(FFileName);
      try
        WriteDataSet0;
        WriteDataSet1;
        WriteDataSet2;
        WriteDataSet3;
        SutraFileWriter.AddFile(sftIcs, FFileName);
      finally
        CloseFile;
      end;
    finally
      FRestartFile.Free;
    end;
  end;

end;

end.
