unit frameSutraLakeUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ImgList,
  Vcl.StdCtrls, UndoItemsScreenObjects,
  frameActivatibleFeatureUnit, System.ImageList;

type
  TframeSutraLake = class(TframeActivatibleFeature)
    pnlCaption: TPanel;
    btnedInitialStage: TButtonedEdit;
    ilLakeButton: TImageList;
    lblInitialStage: TLabel;
    lblInitialU: TLabel;
    btnedInitialU: TButtonedEdit;
    lblFractionRechargeDiverted: TLabel;
    btnedFractionRechargeDiverted: TButtonedEdit;
    btnedFractionDischargeDiverted: TButtonedEdit;
    lblFractionDischargeDiverted: TLabel;
  private
//    FCheckState: TCheckBoxState;
    procedure ClearData;
    { Private declarations }
  public
    procedure GetData(ScreenObjects: TScreenObjectEditCollection);
    procedure SetData(ScreenObjects: TScreenObjectEditCollection; SetAll,
      ClearAll: boolean);
    { Public declarations }
  end;

implementation

uses
  SutraBoundariesUnit, SutraBoundaryUnit, frmGoPhastUnit, ScreenObjectUnit,
  DataSetUnit, PhastModelUnit;

{$R *.dfm}

{#BACKUP EditButtonEnabled.png}
{#BACKUP EditButtonDisabled.png}
{#BACKUP EditButtonPressed.png}
{#BACKUP EditButtonHot.png}

{ TframeSutraLake }

procedure TframeSutraLake.ClearData;
begin
  btnedInitialStage.Text := '';
  btnedInitialU.Text := '';
  btnedFractionRechargeDiverted.Text := '';
  btnedFractionDischargeDiverted.Text := '';
end;

procedure TframeSutraLake.GetData(ScreenObjects: TScreenObjectEditCollection);
var
  LakeList: TSutraLakeList;
  index: Integer;
  ALake: TSutraLake;
  InitialStage: string;
  InitialConcentrationOrTemperature: string;
  FractionRechargeDiverted: string;
  FractionDischargeDiverted: string;
  LakeIndex: Integer;
begin
  LakeList := TSutraLakeList.Create;
  try
    ClearData;
    for index := 0 to ScreenObjects.Count - 1 do
    begin
      ALake := ScreenObjects[index].ScreenObject.SutraBoundaries.Lake;
      if ALake.IsUsed then
      begin
        LakeList.Add(ALake);
      end;
    end;
    if LakeList.Count = 0 then
    begin
      FCheckState := cbUnchecked;
    end
    else if ScreenObjects.Count = LakeList.Count then
    begin
      FCheckState := cbChecked;
    end
    else
    begin
      FCheckState := cbGrayed;
    end;
    if Assigned(OnActivate) then
    begin
      OnActivate(self, FCheckState);
    end;
    if LakeList.Count = 0 then
    begin
      Exit;
    end;

    ALake := LakeList[0];
    InitialStage := ALake.InitialStage;
    InitialConcentrationOrTemperature := ALake.InitialConcentrationOrTemperature;
    FractionRechargeDiverted := ALake.FractionRechargeDiverted;
    FractionDischargeDiverted := ALake.FractionDischargeDiverted;

    for LakeIndex := 0 to LakeList.Count - 1 do
    begin
      ALake := LakeList[LakeIndex];
      if InitialStage <> ALake.InitialStage then
      begin
        InitialStage := '';
      end;
      if InitialConcentrationOrTemperature <> ALake.InitialConcentrationOrTemperature then
      begin
        InitialConcentrationOrTemperature := '';
      end;
      if FractionRechargeDiverted <> ALake.FractionRechargeDiverted then
      begin
        FractionRechargeDiverted := '';
      end;
      if FractionDischargeDiverted <> ALake.FractionDischargeDiverted then
      begin
        FractionDischargeDiverted := '';
      end;
      if (InitialStage = '') and
        (InitialConcentrationOrTemperature = '') and
        (FractionRechargeDiverted = '') and
        (FractionDischargeDiverted = '')
      then
      begin
        break;
      end;
    end;

    btnedInitialStage.Text := InitialStage;
    btnedInitialU.Text := InitialConcentrationOrTemperature;
    btnedFractionRechargeDiverted.Text := FractionRechargeDiverted;
    btnedFractionDischargeDiverted.Text := FractionDischargeDiverted;

  finally
    LakeList.Free;
  end;

end;

procedure TframeSutraLake.SetData(ScreenObjects: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  LakeIndex: Integer;
  ALake: TSutraLake;
  InitialStage: TDataArray;
  InitialU: TDataArray;
  RechargeDiverted: TDataArray;
  DischargeDiverted: TDataArray;
  AScreenObject: TScreenObject;
  DataSetIndex: Integer;
  procedure RemoveLinksToLakeDataSets;
  begin
    if InitialStage <> nil then
    begin
      AScreenObject.RemoveDataSet(InitialStage)
    end;
    if InitialU <> nil then
    begin
      AScreenObject.RemoveDataSet(InitialU)
    end;
    if RechargeDiverted <> nil then
    begin
      AScreenObject.RemoveDataSet(RechargeDiverted)
    end;
    if DischargeDiverted <> nil then
    begin
      AScreenObject.RemoveDataSet(DischargeDiverted)
    end;
  end;
begin
  InitialStage := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KSUTRAInitialLakeS);
  InitialU := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KSUTRAInitialLakeU);
  RechargeDiverted := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KSUTRALakeRecharge);
  DischargeDiverted := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KSUTRALakeDischarge);

  for LakeIndex := 0 to ScreenObjects.Count - 1 do
  begin
    AScreenObject := ScreenObjects[LakeIndex].ScreenObject;
    ALake := AScreenObject.SutraBoundaries.Lake;
    if ClearAll then
    begin
      ALake.IsUsed := False;
    end
    else if SetAll then
    begin
      ALake.IsUsed := True;
    end;

    if ALake.IsUsed then
    begin
      if btnedInitialStage.Text <> '' then
      begin
        ALake.InitialStage := btnedInitialStage.Text;
        if InitialStage <> nil then
        begin
          DataSetIndex := AScreenObject.AddDataSet(InitialStage);
          AScreenObject.DataSetFormulas[DataSetIndex] :=
            ALake.InitialStage;
        end;
      end;
      if btnedInitialU.Text <> '' then
      begin
        ALake.InitialConcentrationOrTemperature := btnedInitialU.Text;
        if InitialU <> nil then
        begin
          DataSetIndex := AScreenObject.AddDataSet(InitialU);
          AScreenObject.DataSetFormulas[DataSetIndex] :=
            ALake.InitialConcentrationOrTemperature;
        end;
      end;
      if btnedFractionRechargeDiverted.Text <> '' then
      begin
        ALake.FractionRechargeDiverted := btnedFractionRechargeDiverted.Text;
        if RechargeDiverted <> nil then
        begin
          DataSetIndex := AScreenObject.AddDataSet(RechargeDiverted);
          AScreenObject.DataSetFormulas[DataSetIndex] :=
            ALake.FractionRechargeDiverted;
        end;
      end;
      if btnedFractionDischargeDiverted.Text <> '' then
      begin
        ALake.FractionDischargeDiverted := btnedFractionDischargeDiverted.Text;
        if DischargeDiverted <> nil then
        begin
          DataSetIndex := AScreenObject.AddDataSet(DischargeDiverted);
          AScreenObject.DataSetFormulas[DataSetIndex] :=
            ALake.FractionDischargeDiverted;
        end;
      end;
    end
    else
    begin
      RemoveLinksToLakeDataSets;
    end;
  end;

end;

end.
