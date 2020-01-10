unit frameScreenObjectCSubUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectTabbedUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, Vcl.ComCtrls, UndoItemsScreenObjects;

type
  TInterbedColumns = (icName, icUsed, icInitialOffset, icThickness,
    icEquivInterbedNumber, icInitialInelasticSpecificStorage,
    icInitialElasticSpecificStorage, icInitialPorosity, icDelayKv,
    icInitialDelayHeadOffset);

  TframeScreenObjectCSub = class(TframeScreenObjectTabbed)
    tabInterbedSystems: TTabSheet;
    rdgSubGroups: TRbwDataGrid4;
  private
    procedure InitializeControls;
    { Private declarations }
  public
    procedure GetData(const List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectCSub: TframeScreenObjectCSub;

implementation

uses
  ModflowPackageSelectionUnit, frmGoPhastUnit, ScreenObjectUnit,
  ModflowCsubUnit;

resourcestring
  StrInterbed = 'Interbed';
  StrUsed = 'Used';
  StrInitialOffset = 'Initial Offset';
  StrThickness = 'Thickness';
  StrEquivalentInterbed = 'Equivalent Interbed Number';
  StrInitialInelasticS = ' Initial Inelastic Specific Storage';
  StrInitialElasticSpec = 'Initial Elastic Specific Storage';
  StrInitialPorosity = 'Initial Porosity';
  StrDelayKv = 'Delay Kv';
  StrInitialDelayHeadO = 'Initial Delay Head Offset';

{$R *.dfm}

{ TframeScreenObjectCSub }

procedure TframeScreenObjectCSub.GetData(
  const List: TScreenObjectEditCollection);
var
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  ModflowCSub: TCSubBoundary;
  FoundFirst: Boolean;
  InterbedIndex: Integer;
  Interbed: TCSubPackageData;
  RowIndex: Integer;
begin
  InitializeControls;
  FoundFirst := False;
  for ScreenObjectIndex := 0 to List.Count - 1 do
  begin
    ScreenObject := List[ScreenObjectIndex].ScreenObject;
    ModflowCSub := ScreenObject.ModflowCSub;
    if (ModflowCSub <> nil) and ModflowCSub.Used then
    begin
      if not FoundFirst then
      begin
        FoundFirst := True;
        for InterbedIndex := 0 to ModflowCSub.CSubPackageData.Count -1 do
        begin
          Interbed := ModflowCSub.CSubPackageData[InterbedIndex];
          RowIndex := rdgSubGroups.Cols[Ord(icName)].IndexOf(Interbed.InterbedSystemName);
          if RowIndex >= 1 then
          begin
            rdgSubGroups.Checked[Ord(icUsed)] := Interbed.Used;
            rdgSubGroups.Cells[Ord(icInitialOffset)] := Interbed.Used;
            
//  TInterbedColumns = (icName, icUsed, icInitialOffset, icThickness,
//    icEquivInterbedNumber, icInitialInelasticSpecificStorage,
//    icInitialElasticSpecificStorage, icInitialPorosity, icDelayKv,
//    icInitialDelayHeadOffset);
          end;
        end;
      end
      else
      begin
      end;
    end;
  end;

end;

procedure TframeScreenObjectCSub.InitializeControls;
var
  Interbeds: TInterbeds;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  Interbeds := frmGoPhast.PhastModel.ModflowPackages.CSubPackage.Interbeds;
  ClearGrid(rdgSubGroups);
  rdgSubGroups.BeginUpdate;
  try
    for ColIndex := 0 to rdgSubGroups.ColCount - 1 do
    begin
      rdgSubGroups.Columns[ColIndex].AutoAdjustColWidths := True;
    end;
    rdgSubGroups.RowCount := Interbeds.Count + 1;
    for RowIndex := 1 to Interbeds.Count do
    begin
      rdgSubGroups.Cells[Ord(icName), RowIndex] :=
        Interbeds[RowIndex-1].Name;
      rdgSubGroups.Objects[Ord(icName), RowIndex] :=
        Interbeds[RowIndex-1];
    end;

    rdgSubGroups.Cells[Ord(icName), 0] := StrInterbed;
    rdgSubGroups.Cells[Ord(icUsed), 0] := StrUsed;
    rdgSubGroups.Cells[Ord(icInitialOffset), 0] := StrInitialOffset;
    rdgSubGroups.Cells[Ord(icThickness), 0] := StrThickness;
    rdgSubGroups.Cells[Ord(icEquivInterbedNumber), 0] := StrEquivalentInterbed;
    rdgSubGroups.Cells[Ord(icInitialInelasticSpecificStorage), 0] := StrInitialInelasticS;
    rdgSubGroups.Cells[Ord(icInitialElasticSpecificStorage), 0] := StrInitialElasticSpec;
    rdgSubGroups.Cells[Ord(icInitialPorosity), 0] := StrInitialPorosity;
    rdgSubGroups.Cells[Ord(icDelayKv), 0] := StrDelayKv;
    rdgSubGroups.Cells[Ord(icInitialDelayHeadOffset), 0] := StrInitialDelayHeadO;
  finally
    rdgSubGroups.EndUpdate;
  end;

  for ColIndex := 0 to rdgSubGroups.ColCount - 1 do
	begin
	  rdgSubGroups.Columns[ColIndex].AutoAdjustColWidths := False;
	end;
  
end;

procedure TframeScreenObjectCSub.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
begin

end;

end.
