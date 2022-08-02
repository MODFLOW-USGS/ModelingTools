unit frameChemSpeciesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameGridUnit,
  Vcl.ExtCtrls, Mt3dmsChemSpeciesUnit, Vcl.Grids, Vcl.StdCtrls;

type
  TSpeciesColumn = (scName, scUseFile, scFileName);

  TframeChemSpecies = class(TFrame)
    spl1: TSplitter;
    frameGridImmobile: TframeGrid;
    frameGridMobile: TframeGrid;
    dlgOpenSelectFile: TOpenDialog;
    procedure frameGridSpeciesGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameGridMobileGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameGridImmobileGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameSpeciesGridStateChange(Sender: TObject; ACol,
      ARow: Integer; const Value: TCheckBoxState);
  private
    FOnEnableTimeControls: TNotifyEvent;
    procedure GetMt3dComponents(Mt3DComponents: TCustomChemSpeciesCollection;
      AFrame: TframeGrid);
    procedure SetMt3dComponents(Mt3DComponents: TCustomChemSpeciesCollection;
      AFrame: TframeGrid);
    procedure EnableTimeControls;
    procedure FixNames(Names: TStringList; AFrame: TframeGrid);
    procedure InitializeSpeciesGrids;
    { Private declarations }
  public
    procedure GetMt3dmsChemSpecies(
      MobileComponents: TMobileChemSpeciesCollection;
      ImmobileComponents: TChemSpeciesCollection);
    procedure SetMt3dmsChemSpecies(
      MobileComponents: TMobileChemSpeciesCollection;
      ImmobileComponents: TChemSpeciesCollection);
    function Mt3dTimeControlsShouldBeEnabled: Boolean;
    property OnEnableTimeControls: TNotifyEvent read FOnEnableTimeControls
      write FOnEnableTimeControls;
    { Public declarations }
  end;

implementation

uses
  PhastModelUnit;

{$R *.dfm}

resourcestring
  StrMobileSpecies = 'Mobile Species';
  StrImmobileSpecies = 'Immobile Species';
  StrUseInitialConcentr = 'Use Initial Concentration File';
  StrFileName = 'File Name';

{ TframeChemSpecies }

procedure TframeChemSpecies.EnableTimeControls;
begin
  if Assigned(OnEnableTimeControls) then
  begin
    OnEnableTimeControls(Self);
  end;
end;

procedure TframeChemSpecies.FixNames(Names: TStringList; AFrame: TframeGrid);
var
  Index: Integer;
  AName: string;
begin
  for Index := 1 to AFrame.seNumber.AsInteger do
  begin
    AName := Trim(AFrame.Grid.Cells[Ord(scName), Index]);
    if AName <> '' then
    begin
      AName := GenerateNewRoot(AName);
      if Names.IndexOf(AName) >= 0 then
      begin
        AFrame.Grid.Cells[Ord(scName), Index] := '';
        AFrame.Grid.Objects[Ord(scName), Index] := nil;
      end
      else
      begin
        Names.Add(AName);
      end;
    end
    else
    begin
      AFrame.Grid.Cells[Ord(scName), Index] := '';
      AFrame.Grid.Objects[Ord(scName), Index] := nil;
    end;
  end;
end;

procedure TframeChemSpecies.frameGridImmobileGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if (ACol = Ord(scFileName))
    and not frameGridImmobile.Grid.Checked[Ord(scUseFile), ARow] then
  begin
    CanSelect := False;
  end;
end;

procedure TframeChemSpecies.frameGridMobileGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if (ACol = Ord(scFileName))
    and not frameGridMobile.Grid.Checked[Ord(scUseFile), ARow] then
  begin
    CanSelect := False;
  end;
end;

procedure TframeChemSpecies.frameSpeciesGridStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  EnableTimeControls;
end;

procedure TframeChemSpecies.frameGridSpeciesGridButtonClick(Sender: TObject;
  ACol, ARow: Integer);
var
  Grid: TStringGrid;
begin
  inherited;
  Grid := Sender as TStringGrid;
  dlgOpenSelectFile.FileName := Grid.Cells[ACol, ARow];
  if dlgOpenSelectFile.Execute then
  begin
    Grid.Cells[ACol, ARow] := dlgOpenSelectFile.FileName;
  end;
end;

procedure TframeChemSpecies.GetMt3dComponents(
  Mt3DComponents: TCustomChemSpeciesCollection; AFrame: TframeGrid);
var
  Item: TChemSpeciesItem;
  Index: Integer;
  RowIndex: Integer;
begin
  AFrame.seNumber.AsInteger := Mt3DComponents.Count;
  AFrame.Grid.BeginUpdate;
  try
    for RowIndex := 1 to AFrame.Grid.RowCount - 1 do
    begin
      AFrame.Grid.Objects[Ord(scName), RowIndex] := nil;
    end;
    if Mt3DComponents.Count > 0 then
    begin
      for Index := 0 to Mt3DComponents.Count - 1 do
      begin
        Item := Mt3DComponents[Index];
        AFrame.Grid.Cells[Ord(scName), Index + 1] := Item.Name;
        AFrame.Grid.Checked[Ord(scUseFile), Index + 1] := Item.UseInitialConcentrationFile;
        AFrame.Grid.Cells[Ord(scFileName), Index + 1] := Item.InitialConcentrationFileName;
        AFrame.Grid.Objects[Ord(scName), Index + 1] := Item;
      end;
    end
    else
    begin
      AFrame.Grid.Cells[Ord(scName), 1] := '';
        AFrame.Grid.Objects[Ord(scName), 1] := nil;
    end;
  finally
    AFrame.Grid.EndUpdate;
  end;
  AFrame.seNumber.AsInteger := Mt3DComponents.Count;
end;

procedure TframeChemSpecies.GetMt3dmsChemSpecies(
  MobileComponents: TMobileChemSpeciesCollection;
  ImmobileComponents: TChemSpeciesCollection);
var
  RowIndex: Integer;
begin
  InitializeSpeciesGrids;
  
  for RowIndex := 1 to frameGridMobile.Grid.RowCount - 1 do
  begin
    frameGridMobile.Grid.Objects[Ord(scName),RowIndex] := nil;
  end;
  for RowIndex := 1 to frameGridImmobile.Grid.RowCount - 1 do
  begin
    frameGridImmobile.Grid.Objects[Ord(scName),RowIndex] := nil;
  end;
  GetMt3dComponents(MobileComponents, frameGridMobile);
  GetMt3dComponents(ImmobileComponents, frameGridImmobile);
  EnableTimeControls;
end;

procedure TframeChemSpecies.InitializeSpeciesGrids;
begin
  frameGridMobile.Grid.Cells[Ord(scName), 0] := StrMobileSpecies;
  frameGridMobile.Grid.Cells[Ord(scUseFile), 0] := StrUseInitialConcentr;
  frameGridMobile.Grid.Cells[Ord(scFileName), 0] := StrFileName;

  frameGridImmobile.Grid.Cells[Ord(scName), 0] := StrImmobileSpecies;
  frameGridImmobile.Grid.Cells[Ord(scUseFile), 0] := StrUseInitialConcentr;
  frameGridImmobile.Grid.Cells[Ord(scFileName), 0] := StrFileName;
end;

function TframeChemSpecies.Mt3dTimeControlsShouldBeEnabled: Boolean;
var
  RowIndex: Integer;
begin
  result := False;
  for RowIndex := 1 to frameGridMobile.Grid.RowCount - 1 do
  begin
    result := frameGridMobile.Grid.Checked[Ord(scUseFile), RowIndex];
    if result then
    begin
      break;
    end;
  end;
  if not result then
  begin
    for RowIndex := 1 to frameGridImmobile.Grid.RowCount - 1 do
    begin
      result := frameGridImmobile.Grid.Checked[Ord(scUseFile), RowIndex];
      if result then
      begin
        break;
      end;
    end;
  end;
end;

procedure TframeChemSpecies.SetMt3dComponents(
  Mt3DComponents: TCustomChemSpeciesCollection; AFrame: TframeGrid);
var
  ItemIndex: Integer;
  Index: Integer;
  Item: TChemSpeciesItem;
  AList: TList;
  GridCol: TStrings;
  ItemRow: integer;
begin
  AList := TList.Create;
  try
    for Index := 1 to AFrame.seNumber.AsInteger do
    begin
      if Trim(AFrame.Grid.Cells[Ord(scName), Index]) <> '' then
      begin
        AList.Add(AFrame.Grid.Objects[Ord(scName), Index]);
      end;
    end;
    GridCol := AFrame.Grid.Cols[Ord(scName)];
    for Index := Mt3DComponents.Count - 1 downto 0 do
    begin
      if AList.IndexOf(Mt3DComponents[Index]) < 0 then
      begin
        Item := Mt3DComponents[Index];
        ItemRow := GridCol.IndexOfObject(Item);
        if ItemRow >= 1 then
        begin
          AFrame.Grid.Objects[Ord(scName), ItemRow] := nil;
        end;
        Mt3DComponents.Delete(Index);
      end;
    end;
    ItemIndex := 0;
    for Index := 1 to AFrame.seNumber.AsInteger do
    begin
      if (AFrame.Grid.Objects[Ord(scName), Index] = nil)
        and (Trim(AFrame.Grid.Cells[Ord(scName), Index]) <> '') then
      begin
        Item := Mt3DComponents.Add;
      end
      else
      begin
        Item := AFrame.Grid.Objects[Ord(scName), Index] as TChemSpeciesItem;
      end;
      if Item <> nil then
      begin
        Item.Index := ItemIndex;
        Item.Name := Trim(AFrame.Grid.Cells[Ord(scName), Index]);
        Item.UseInitialConcentrationFile := AFrame.Grid.Checked[Ord(scUseFile), Index];
        Item.InitialConcentrationFileName := AFrame.Grid.Cells[Ord(scFileName), Index];

        Inc(ItemIndex);
      end;
    end;
  finally
    AList.Free;
  end;
end;

procedure TframeChemSpecies.SetMt3dmsChemSpecies(
  MobileComponents: TMobileChemSpeciesCollection;
  ImmobileComponents: TChemSpeciesCollection);
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    Names.Sorted := True;
    Names.CaseSensitive := False;
    FixNames(Names, frameGridMobile);
    FixNames(Names, frameGridImmobile);
  finally
    Names.Free;
  end;

  SetMt3dComponents(MobileComponents, frameGridMobile);
  SetMt3dComponents(ImmobileComponents, frameGridImmobile);
end;

end.
