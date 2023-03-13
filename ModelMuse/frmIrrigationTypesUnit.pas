unit frmIrrigationTypesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.ExtCtrls,
  frameGridUnit, Vcl.StdCtrls, Vcl.Buttons, UndoItems, ModflowFmpIrrigationUnit;

type
  TIrrigationColumn = (icNumber, icName {, icEfficiency});

  TfrmIrrigationTypes = class(TfrmCustomGoPhast)
    frameIrrigationTypes: TframeGrid;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure frameIrrigationTypesGridEndUpdate(Sender: TObject);
    procedure frameIrrigationTypesseNumberChange(Sender: TObject);
  private
    FIrrigationTypes: TIrrigationCollection;
    FGettingData: Boolean;
    procedure GetData;
    procedure SetData;
    procedure InitializeGrid;
    { Private declarations }
  public
    destructor Destroy; override;
    { Public declarations }
  end;

  TUndoSetIrrigationTypes = class(TCustomUndo)
  private
    FNewIrrigationTypes: TIrrigationCollection;
    FOldIrrigationTypes: TIrrigationCollection;
  protected
    function Description: string; override;
  public
    constructor Create(var IrrigationTypes: TIrrigationCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmIrrigationTypes: TfrmIrrigationTypes;

implementation

uses
  frmGoPhastUnit;

{$R *.dfm}

{ TfrmModflowIrrigationTypes }

procedure TfrmIrrigationTypes.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

destructor TfrmIrrigationTypes.Destroy;
begin
  FIrrigationTypes.Free;
  inherited;
end;

procedure TfrmIrrigationTypes.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmIrrigationTypes.frameIrrigationTypesGridEndUpdate(
  Sender: TObject);
begin
  inherited;
  if not FGettingData then
  begin
    frameIrrigationTypes.GridEndUpdate(Sender);
  end;
end;

procedure TfrmIrrigationTypes.frameIrrigationTypesseNumberChange(
  Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  frameIrrigationTypes.seNumberChange(Sender);
  for RowIndex := 1 to frameIrrigationTypes.Grid.RowCount - 1 do
  begin
    frameIrrigationTypes.Grid.Cells[Ord(icNumber), RowIndex] :=
      IntToStr(RowIndex);
  end;
end;

procedure TfrmIrrigationTypes.InitializeGrid;
begin
  frameIrrigationTypes.Grid.Cells[Ord(icName), 0] := 'Irrigation Type';
//  frameIrrigationTypes.Grid.Cells[Ord(icEfficiency), 0] := 'Efficiency';
  frameIrrigationTypes.Grid.Cells[Ord(icNumber), 1] := '1';
end;

procedure TfrmIrrigationTypes.GetData;
var
  Index: Integer;
begin
  FGettingData := True;
  try
    FIrrigationTypes := TIrrigationCollection.Create(nil);
    FIrrigationTypes.Assign(frmGoPhast.PhastModel.IrrigationTypes);
    frameIrrigationTypes.Grid.BeginUpdate;
    try
      InitializeGrid;
      frameIrrigationTypes.seNumber.AsInteger := FIrrigationTypes.Count;
      for Index := 0 to FIrrigationTypes.Count - 1 do
      begin
        frameIrrigationTypes.Grid.Cells[Ord(icNumber), Index+1] := IntToStr(Index+1);
        frameIrrigationTypes.Grid.Cells[Ord(icName), Index+1] := FIrrigationTypes[Index].Name;
//        frameIrrigationTypes.Grid.RealValue[Ord(icEfficiency), Index+1] := FIrrigationTypes[Index].Efficiency;

        frameIrrigationTypes.Grid.Objects[Ord(icName), Index+1] := FIrrigationTypes[Index];
      end;
    finally
      frameIrrigationTypes.Grid.EndUpdate;
    end;
  finally
    FGettingData := False;
  end;
end;

procedure TfrmIrrigationTypes.SetData;
var
  ItemList: TList;
  AnObject: TObject;
  IrrigationItem: TIrrigationItem;
  RowIndex: Integer;
  ItemIndex: Integer;
begin
  ItemList := TList.Create;
  try
    for RowIndex := 1 to frameIrrigationTypes.seNumber.AsInteger do
    begin
      AnObject := frameIrrigationTypes.Grid.Objects[Ord(icName), RowIndex];
      if AnObject <> nil then
      begin
        ItemList.Add(AnObject);
      end;
    end;
    for ItemIndex := 0 to FIrrigationTypes.Count - 1 do
    begin
      IrrigationItem := FIrrigationTypes[ItemIndex];
      if ItemList.IndexOf(IrrigationItem) < 0 then
      begin
        IrrigationItem.Free;
      end;
    end;
  finally
    ItemList.Free;
  end;

  for RowIndex := 1 to frameIrrigationTypes.seNumber.AsInteger do
  begin
    AnObject := frameIrrigationTypes.Grid.Objects[Ord(icName), RowIndex];
    if AnObject = nil then
    begin
      IrrigationItem := FIrrigationTypes.Add as TIrrigationItem;
    end
    else
    begin
      IrrigationItem := AnObject as TIrrigationItem;
    end;
    IrrigationItem.Index := RowIndex -1;
    IrrigationItem.Name := frameIrrigationTypes.Grid.Cells[Ord(icName), RowIndex];
//    IrrigationItem.Efficiency := frameIrrigationTypes.Grid.RealValueDefault[Ord(icEfficiency), RowIndex, 0];
  end;

  if FIrrigationTypes.IsSame(frmGoPhast.PhastModel.IrrigationTypes) then
  begin
    FreeAndNil(FIrrigationTypes);
  end
  else
  begin
    frmGoPhast.UndoStack.Submit(TUndoSetIrrigationTypes.Create(FIrrigationTypes));
  end;
end;

{ TUndoSetIrrigationTypes }

constructor TUndoSetIrrigationTypes.Create(
  var IrrigationTypes: TIrrigationCollection);
begin
  FNewIrrigationTypes := IrrigationTypes;
  IrrigationTypes := nil;
  FOldIrrigationTypes := TIrrigationCollection.Create(nil);
  FOldIrrigationTypes.Assign(frmGoPhast.PhastModel.IrrigationTypes);
end;

function TUndoSetIrrigationTypes.Description: string;
begin
  result := 'change irrigation types';
end;

destructor TUndoSetIrrigationTypes.Destroy;
begin
  FNewIrrigationTypes.Free;
  FOldIrrigationTypes.Free;
  inherited;
end;

procedure TUndoSetIrrigationTypes.DoCommand;
begin
  frmGoPhast.PhastModel.IrrigationTypes := FNewIrrigationTypes
end;

procedure TUndoSetIrrigationTypes.Undo;
begin
  frmGoPhast.PhastModel.IrrigationTypes := FOldIrrigationTypes
end;

end.
