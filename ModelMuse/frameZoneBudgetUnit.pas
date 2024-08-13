unit frameZoneBudgetUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, Mask, JvExMask, JvSpin,
  Grids, RbwDataGrid4, ModflowPackageSelectionUnit;

type
  TframeZoneBudget = class(TframePackage)
    lblCompositeZones: TLabel;
    rdgCompositeZones: TRbwDataGrid4;
    seNumberOfZones: TJvSpinEdit;
    lblNumberOfZones: TLabel;
    btnInsertZone: TButton;
    btnDeleteZone: TButton;
    cbExportZblst: TCheckBox;
    cbExportCsv: TCheckBox;
    cbExportCsv2: TCheckBox;
    procedure seNumberOfZonesChange(Sender: TObject);
    procedure rdgCompositeZonesEndUpdate(Sender: TObject);
    procedure btnInsertZoneClick(Sender: TObject);
    procedure btnDeleteZoneClick(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure cbExportZblstClick(Sender: TObject);
    procedure cbExportCsvClick(Sender: TObject);
    procedure cbExportCsv2Click(Sender: TObject);
    procedure rdgCompositeZonesBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
  private
    FGettingData: Boolean;
    procedure UpdateSpinEdit;
    procedure EnableDeleteButton;
    procedure CheckOutputSelected;
    procedure ZoneBudgetWarning;
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

implementation

uses
  IntListUnit, frmGoPhastUnit, GoPhastTypes;

resourcestring
  StrZoneName = 'Zone name';
  StrAtLeastOneOutput = 'At least one output file for ZONEBUDGET must be sel' +
  'ected';

{$R *.dfm}

{ TframeZoneBudget }

procedure TframeZoneBudget.btnDeleteZoneClick(Sender: TObject);
begin
  inherited;
  if rdgCompositeZones.Row <= 0 then
  begin
    Exit;
  end;
  if rdgCompositeZones.RowCount > 2 then
  begin
    rdgCompositeZones.DeleteRow(rdgCompositeZones.Row);
  end
  else
  begin
    rdgCompositeZones.Cells[0,1] := '';
    rdgCompositeZones.Cells[1,1] := '';
  end;
  UpdateSpinEdit;
end;

procedure TframeZoneBudget.btnInsertZoneClick(Sender: TObject);
begin
  inherited;
  if rdgCompositeZones.Row >= 1 then
  begin
    rdgCompositeZones.InsertRow(rdgCompositeZones.Row);
    UpdateSpinEdit;
  end;
end;

procedure TframeZoneBudget.cbExportCsv2Click(Sender: TObject);
begin
  inherited;
  CheckOutputSelected;
end;

procedure TframeZoneBudget.cbExportCsvClick(Sender: TObject);
begin
  inherited;
  CheckOutputSelected;
end;

procedure TframeZoneBudget.cbExportZblstClick(Sender: TObject);
begin
  inherited;
  CheckOutputSelected;
end;

procedure TframeZoneBudget.GetData(Package: TModflowPackageSelection);
var
  ZoneBudgetPackage: TZoneBudgetSelect;
  CompositeIndex: Integer;
  CompositeZoneItem: TCompositeZoneItem;
  CompositeZone: TCompositeZone;
  ZoneIndex: Integer;
  Zone: TZoneItem;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  FGettingData := True;
  try
    ZoneBudgetPackage := Package as TZoneBudgetSelect;

    cbExportZblst.Checked := ZoneBudgetPackage.ExportZBLST;
    cbExportCsv.Checked := ZoneBudgetPackage.ExportCSV;
    cbExportCsv2.Checked := ZoneBudgetPackage.ExportCSV2;

    rdgCompositeZones.BeginUpdate;
    try
      for RowIndex := 1 to rdgCompositeZones.RowCount - 1 do
      begin
        for ColIndex := 0 to rdgCompositeZones.ColCount - 1 do
        begin
          rdgCompositeZones.Cells[ColIndex,RowIndex] := '';
        end;
      end;
      seNumberOfZones.AsInteger := ZoneBudgetPackage.CompositeZones.Count;
      for CompositeIndex := 0 to ZoneBudgetPackage.CompositeZones.Count - 1 do
      begin
        RowIndex := CompositeIndex+1;
        CompositeZoneItem := ZoneBudgetPackage.CompositeZones[CompositeIndex];
        CompositeZone := CompositeZoneItem.CompositeZone;
        rdgCompositeZones.Cells[0,RowIndex] := CompositeZone.ZoneName;
        for ZoneIndex := 0 to CompositeZone.Count - 1 do
        begin
          ColIndex := ZoneIndex + 1;
          Zone := CompositeZone[ZoneIndex];
          rdgCompositeZones.Cells[ColIndex,RowIndex]
            := IntToStr(Zone.ZoneNumber);
        end;
      end;
    finally
      rdgCompositeZones.EndUpdate;
    end;
  finally
    FGettingData := False;
  end;
  rcSelectionControllerEnabledChange(nil);
end;

procedure TframeZoneBudget.Loaded;
begin
  inherited;
  rdgCompositeZones.Cells[0,0] := StrZoneName;
end;

procedure TframeZoneBudget.UpdateSpinEdit;
var
  NumZones: Integer;
begin
  if seNumberOfZones <> nil then
  begin
    NumZones := rdgCompositeZones.RowCount - 1;
    if (NumZones = 1)
      and (Trim(rdgCompositeZones.Cells[0, 1]) = '')
      and (Trim(rdgCompositeZones.Cells[1, 1]) = '') then
    begin
      NumZones := 0;
    end;
    if seNumberOfZones.AsInteger <> NumZones then
    begin
      seNumberOfZones.AsInteger := NumZones;
      seNumberOfZonesChange(nil);
    end;
  end;
end;

procedure TframeZoneBudget.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  EnableDeleteButton;
  cbExportZblst.Enabled := rcSelectionController.Enabled
    and (frmGoPhast.ModelSelection <> msModflow2015);
  cbExportCsv.Enabled := cbExportZblst.Enabled;
  cbExportCsv2.Enabled := cbExportZblst.Enabled;
  rdgCompositeZones.Enabled := cbExportZblst.Enabled;
  seNumberOfZones.Enabled := cbExportZblst.Enabled;
  btnInsertZone.Enabled := cbExportZblst.Enabled;
end;

procedure TframeZoneBudget.rdgCompositeZonesBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  if (ACol = 0) and (ARow > 1) then
  begin
    if rdgCompositeZones.Cols[0].IndexOf(
      rdgCompositeZones.Cells[ACol, ARow]) <> ARow then
    begin
      rdgCompositeZones.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TframeZoneBudget.rdgCompositeZonesEndUpdate(Sender: TObject);
begin
  inherited;
  UpdateSpinEdit;
end;

procedure TframeZoneBudget.seNumberOfZonesChange(Sender: TObject);
var
  NewNumberOfZones: Integer;
  Index: Integer;
begin
  inherited;
  EnableDeleteButton;
  NewNumberOfZones := seNumberOfZones.AsInteger;
  if NewNumberOfZones > 0 then
  begin
    rdgCompositeZones.Color := clWindow;
    rdgCompositeZones.RowCount := NewNumberOfZones+1;
    for Index := 1 to rdgCompositeZones.RowCount - 1 do
    begin
      if rdgCompositeZones.Cells[0,Index] = '' then
      begin
        rdgCompositeZones.Cells[0,Index] := Format('CZ%d', [Index]);
      end;
    end;
  end
  else
  begin
    rdgCompositeZones.Color := clBtnFace;
    rdgCompositeZones.RowCount := 2;
    for Index := 0 to rdgCompositeZones.ColCount - 1 do
    begin
      rdgCompositeZones.Cells[Index,1] := '';
    end;
  end;
end;

procedure TframeZoneBudget.SetData(Package: TModflowPackageSelection);
var
  ZoneBudgetPackage: TZoneBudgetSelect;
  CompositeZoneCount: Integer;
  RowIndex: Integer;
  CompositeZoneItem: TCompositeZoneItem;
  CompositeZone: TCompositeZone;
  ColIndex: Integer;
  AZone: Integer;
  ZoneItem: TZoneItem;
  Zones: TIntegerList;
  ZoneIndex: Integer;
begin
  inherited;
  ZoneBudgetPackage := Package as TZoneBudgetSelect;

  ZoneBudgetPackage.ExportZBLST := cbExportZblst.Checked;
  ZoneBudgetPackage.ExportCSV := cbExportCsv.Checked;
  ZoneBudgetPackage.ExportCSV2 := cbExportCsv2.Checked;

  if seNumberOfZones.AsInteger = 0 then
  begin
    ZoneBudgetPackage.CompositeZones.Clear;
  end
  else
  begin
    Zones:= TIntegerList.Create;
    try
      CompositeZoneCount := 0;
      for RowIndex := 0 to rdgCompositeZones.RowCount - 1 do
      begin
        if (rdgCompositeZones.Cells[0,RowIndex] <> '')
          and (rdgCompositeZones.Cells[1,RowIndex] <> '') then
        begin
          Inc(CompositeZoneCount);

          if CompositeZoneCount > ZoneBudgetPackage.CompositeZones.Count then
          begin
            CompositeZoneItem := ZoneBudgetPackage.
              CompositeZones.Add as TCompositeZoneItem;
          end
          else
          begin
            CompositeZoneItem := ZoneBudgetPackage.
              CompositeZones[CompositeZoneCount-1];
          end;
          CompositeZone := CompositeZoneItem.CompositeZone;
          CompositeZone.ZoneName := rdgCompositeZones.Cells[0,RowIndex];

          Zones.Clear;
          for ColIndex := 1 to rdgCompositeZones.ColCount - 1 do
          begin
            if TryStrToInt(rdgCompositeZones.Cells[ColIndex,RowIndex], AZone) then
            begin
              Zones.AddUnique(AZone);
            end;
          end;

          for ZoneIndex := 0 to Zones.Count - 1 do
          begin
            if ZoneIndex < CompositeZone.Count then
            begin
              ZoneItem := CompositeZone[ZoneIndex];
            end
            else
            begin
              ZoneItem := CompositeZone.Add as TZoneItem;
            end;
            ZoneItem.ZoneNumber := Zones[ZoneIndex];
          end;

          while CompositeZone.Count > Zones.Count do
          begin
            CompositeZone.Delete(CompositeZone.Count-1);
          end;
        end;
      end;
    finally
      Zones.Free;
    end;
    while ZoneBudgetPackage.CompositeZones.Count > CompositeZoneCount do
    begin
      ZoneBudgetPackage.CompositeZones.Delete(
        ZoneBudgetPackage.CompositeZones.Count-1);
    end;
  end;
  ZoneBudgetWarning;
end;

procedure TframeZoneBudget.ZoneBudgetWarning;
begin
  if Selected and not FGettingData then
  begin
    if not cbExportZblst.Checked
      and not cbExportCsv.Checked
      and not cbExportCsv2.Checked then
    begin
      Beep;
      MessageDlg(StrAtLeastOneOutput,
        mtError, [mbOK], 0);
    end;

  end;
end;

procedure TframeZoneBudget.CheckOutputSelected;
begin
  if not cbExportZblst.Checked
    and not cbExportCsv.Checked and
    not cbExportCsv2.Checked then
  begin
    cbExportZblst.Font.Color := clRed;
    cbExportCsv.Font.Color := clRed;
    cbExportCsv2.Font.Color := clRed;
    cbExportZblst.Font.Style := cbExportCsv2.Font.Style + [fsBold];
    cbExportCsv.Font.Style := cbExportCsv2.Font.Style + [fsBold];
    cbExportCsv2.Font.Style := cbExportCsv2.Font.Style + [fsBold];
  end
  else
  begin
    cbExportZblst.Font.Color := clWindowText;
    cbExportCsv.Font.Color := clWindowText;
    cbExportCsv2.Font.Color := clWindowText;
    cbExportZblst.Font.Style := cbExportCsv2.Font.Style - [fsBold];
    cbExportCsv.Font.Style := cbExportCsv2.Font.Style - [fsBold];
    cbExportCsv2.Font.Style := cbExportCsv2.Font.Style - [fsBold];
  end;
  ZoneBudgetWarning;
end;

procedure TframeZoneBudget.EnableDeleteButton;
var
  NewNumberOfZones: Integer;
begin
  NewNumberOfZones := seNumberOfZones.AsInteger;
  btnDeleteZone.Enabled := rcSelectionController.Enabled
    and (NewNumberOfZones > 0) and (frmGoPhast.ModelSelection <> msModflow2015);
end;

end.
