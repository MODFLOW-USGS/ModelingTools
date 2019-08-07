unit frameScreenObjectFootprintWellUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, RbwEdit,
  UndoItemsScreenObjects, ScreenObjectUnit;

type
  TframeScreenObjectFootprintWell = class(TFrame)
    cbUseFootprintWell: TCheckBox;
    lblPumpingRate: TLabel;
    edPumpingRate: TRbwEdit;
    btnPumpingRate: TButton;
    procedure cbUseFootprintWellClick(Sender: TObject);
  private
    FGettingData: Boolean;
    procedure FillListOfScreenObjects(ListOfScreenObjects: TScreenObjectList;
      List: TScreenObjectEditCollection);
    { Private declarations }
  public
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection);
    { Public declarations }
  end;

implementation

uses
  GoPhastTypes, FootprintBoundary, DataSetUnit, PhastModelUnit, frmGoPhastUnit;



{$R *.dfm}

procedure TframeScreenObjectFootprintWell.cbUseFootprintWellClick(
  Sender: TObject);
begin
  if not FGettingData then
  begin
    cbUseFootprintWell.AllowGrayed := false;
  end;
  lblPumpingRate.Enabled := cbUseFootprintWell.State <> cbUnchecked;
  edPumpingRate.Enabled := cbUseFootprintWell.State <> cbUnchecked;
  btnPumpingRate.Enabled := cbUseFootprintWell.State <> cbUnchecked;
end;

procedure TframeScreenObjectFootprintWell.FillListOfScreenObjects(
  ListOfScreenObjects: TScreenObjectList; List: TScreenObjectEditCollection);
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    if (ScreenObject.ViewDirection = vdTop) and
      (ScreenObject.Count = ScreenObject.SectionCount) then
    begin
      ListOfScreenObjects.Add(ScreenObject);
    end;
  end;
end;

procedure TframeScreenObjectFootprintWell.GetData(
  List: TScreenObjectEditCollection);
var
  ListOfScreenObjects: TScreenObjectList;
  FirstScreenObject: TScreenObject;
  index: Integer;
  AScreenObject: TScreenObject;
begin
  FGettingData := true;
  try
    edPumpingRate.Text := '';
    cbUseFootprintWell.AllowGrayed := False;
    ListOfScreenObjects := TScreenObjectList.Create;
    try
      FillListOfScreenObjects(ListOfScreenObjects, List);
      if ListOfScreenObjects.Count = 0 then
      begin
        cbUseFootprintWell.Checked := False;
        cbUseFootprintWell.Enabled := false;
        Exit;
      end
      else
      begin
        cbUseFootprintWell.Enabled := True;
      end;

      FirstScreenObject := ListOfScreenObjects[0];
      for index := 0 to ListOfScreenObjects.Count -1 do
      begin
        AScreenObject := ListOfScreenObjects[index];
        if (AScreenObject.FootprintWell <> nil)
          and AScreenObject.FootprintWell.Used then
        begin
          FirstScreenObject := AScreenObject;
          Break;
        end;
      end;
      cbUseFootprintWell.Checked := (FirstScreenObject.FootprintWell <> nil)
          and FirstScreenObject.FootprintWell.Used;
      if not cbUseFootprintWell.Checked then
      begin
        cbUseFootprintWell.AllowGrayed := False;
        Exit;
      end;
      edPumpingRate.Text := FirstScreenObject.FootprintWell.Withdrawal;
      for index := 0 to ListOfScreenObjects.Count -1 do
      begin
        AScreenObject := ListOfScreenObjects[index];
        if (AScreenObject.FootprintWell <> nil)
          and AScreenObject.FootprintWell.Used then
        begin
          if AScreenObject.FootprintWell.Withdrawal <> edPumpingRate.Text then
          begin
            edPumpingRate.Text := '';
          end;
        end
        else
        begin
          cbUseFootprintWell.AllowGrayed := True;
          cbUseFootprintWell.State := cbGrayed;
        end;
        if (cbUseFootprintWell.State = cbGrayed)
          and (edPumpingRate.Text = '') then
        begin
          break;
        end;
      end;
    finally
      ListOfScreenObjects.Free;
    end;
  finally
    FGettingData := False;
  end;
end;

procedure TframeScreenObjectFootprintWell.SetData(
  List: TScreenObjectEditCollection);
var
  SetAll, ClearAll: boolean;
  ListOfScreenObjects: TScreenObjectList;
  index: Integer;
  AScreenObject: TScreenObject;
  FootprintWell: TFootprintWell;
  BoundaryPumpage: TDataArray;
  DataArrayPosition: Integer;
begin
  SetAll := cbUseFootprintWell.State = cbChecked;
  ClearAll := cbUseFootprintWell.State = cbUnChecked;
  ListOfScreenObjects := TScreenObjectList.Create;
  try
    FillListOfScreenObjects(ListOfScreenObjects, List);
    if ListOfScreenObjects.Count = 0 then
    begin
      Exit;
    end;
    BoundaryPumpage := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KWithdrawals);

    for index := 0 to ListOfScreenObjects.Count - 1 do
    begin
      AScreenObject := ListOfScreenObjects[index];

      if ClearAll then
      begin
        if (AScreenObject.FootprintWell <> nil)
          and AScreenObject.FootprintWell.Used then
        begin
          AScreenObject.RemoveDataSet(BoundaryPumpage)
        end;
        AScreenObject.FootprintWell := nil;
      end
      else
      begin
        FootprintWell := AScreenObject.FootprintWell;
        if SetAll then
        begin
          FootprintWell.IsUsed := True;
        end;
        if FootprintWell.Used and (edPumpingRate.Text <> '') then
        begin
          FootprintWell.Withdrawal := edPumpingRate.Text;
          DataArrayPosition := AScreenObject.AddDataSet(BoundaryPumpage);
          AScreenObject.DataSetFormulas[DataArrayPosition] := edPumpingRate.Text;
        end
        else
        begin
          if (AScreenObject.FootprintWell <> nil)
            and AScreenObject.FootprintWell.Used then
          begin
            AScreenObject.RemoveDataSet(BoundaryPumpage)
          end;
          AScreenObject.RemoveDataSet(BoundaryPumpage)
        end;
      end;
    end;
  finally
    ListOfScreenObjects.Free;
  end;
end;

end.
