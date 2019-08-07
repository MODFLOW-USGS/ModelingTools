unit frmLinkRasterUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, frameGridUnit, StdCtrls,
  Buttons, ExtCtrls, UndoItems, LinkedRastersUnit;

type
  TRasterColumns = (rcRasterName, rcFileName, rcType);

  TfrmLinkRaster = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    frameRasters: TframeGrid;
    dlgOpenFile: TOpenDialog;
    procedure FormCreate(Sender: TObject); override;
    procedure frameRastersGridButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure frameRastersGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure btnOKClick(Sender: TObject);
  private
    procedure InitializeControls;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoLinkedRasters = class(TCustomUndo)
  private
    FOldRasters: TLinkedRasterCollection;
    FNewRasters: TLinkedRasterCollection;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Undo; override;
    constructor Create(var Rasters: TLinkedRasterCollection);
    destructor Destroy; override;
  end;

var
  frmLinkRaster: TfrmLinkRaster;

implementation

uses
  frmGoPhastUnit, RbwDataGrid4;

resourcestring
  StrRasterName = 'Raster Name';
  StrFileName = 'File Name';
  StrFileType = 'File Type';
  StrChangeLinkedRaster = 'change linked rasters';

{$R *.dfm}

procedure TfrmLinkRaster.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmLinkRaster.FormCreate(Sender: TObject);
begin
  inherited;
  frameRasters.Grid.BeginUpdate;
  try
    InitializeControls;
    GetData;
  finally
    frameRasters.Grid.EndUpdate;
  end;
end;

procedure TfrmLinkRaster.InitializeControls;
begin
  frameRasters.Grid.Cells[Ord(rcRasterName), 0] := StrRasterName;
  frameRasters.Grid.Cells[Ord(rcFileName), 0] := StrFileName;
  frameRasters.Grid.Cells[Ord(rcType), 0] := StrFileType;
end;

procedure TfrmLinkRaster.SetData;
var
  Dummy: TNotifyEvent;
  Grid: TRbwDataGrid4;
  Rasters: TLinkedRasterCollection;
  RowIndex: Integer;
  RasterItem: TLinkedRasterItem;
begin
  Dummy := nil;
  Rasters := TLinkedRasterCollection.Create(Dummy);
  try
    Grid := frameRasters.Grid;
    for RowIndex := 1 to frameRasters.seNumber.AsInteger do
    begin
      if (Grid.Cells[Ord(rcRasterName), RowIndex] <> '')
        and (Grid.Cells[Ord(rcFileName), RowIndex] <> '')
        and (Grid.ItemIndex[Ord(rcType), RowIndex] >= 0) then
      begin
        RasterItem := Rasters.Add;
        RasterItem.RasterName := Grid.Cells[Ord(rcRasterName), RowIndex];
        RasterItem.FileName := Grid.Cells[Ord(rcFileName), RowIndex];
        RasterItem.RasterType := TRasterType(Grid.ItemIndex[Ord(rcType), RowIndex]);
      end;
    end;
    frmGoPhast.UndoStack.Submit(TUndoLinkedRasters.Create(Rasters));
  finally
    Rasters.Free;
  end;
end;

procedure TfrmLinkRaster.frameRastersGridBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  if (ARow >= 1) then
  begin

    if ACol = Ord(rcRasterName) then
    begin
      if (frameRasters.Grid.Cells[Ord(rcFileName), ARow] <> '')
        and (frameRasters.Grid.Cells[Ord(rcRasterName), ARow] = '') then
      begin
        frameRasters.Grid.Canvas.Brush.Color := clRed;
      end;
    end
    else if ACol = Ord(rcFileName) then
    begin
      if (frameRasters.Grid.Cells[Ord(rcFileName), ARow] <> '') and
        not FileExists(frameRasters.Grid.Cells[Ord(rcFileName), ARow]) then
      begin
        frameRasters.Grid.Canvas.Brush.Color := clRed;
      end;
    end;
  end;
end;

procedure TfrmLinkRaster.frameRastersGridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  FilterIndex: Integer;
  NewName: string;
begin
  inherited;
  FilterIndex := frameRasters.Grid.ItemIndex[Ord(rcType), ARow]+1;
  if FilterIndex in [1,2] then
  begin
    dlgOpenFile.FilterIndex := FilterIndex;
  end;
  dlgOpenFile.FileName := frameRasters.Grid.Cells[ACol, ARow];
  if dlgOpenFile.Execute then
  begin
    frameRasters.Grid.Cells[ACol, ARow] := dlgOpenFile.FileName;
    frameRasters.Grid.ItemIndex[Ord(rcType), ARow] := dlgOpenFile.FilterIndex-1;
    if frameRasters.Grid.Cells[Ord(rcRasterName), ARow] = '' then
    begin
      NewName := ExtractFileName(dlgOpenFile.FileName);
      NewName := ChangeFileExt(NewName, '');
      frameRasters.Grid.Cells[Ord(rcRasterName), ARow] := NewName;
    end;
  end;
end;

procedure TfrmLinkRaster.GetData;
var
  Rasters: TLinkedRasterCollection;
  RasterIndex: Integer;
  RasterItem: TLinkedRasterItem;
  Grid: TRbwDataGrid4;
begin
  Rasters := frmGoPhast.PhastModel.LinkedRasters;
  frameRasters.seNumber.AsInteger := Rasters.Count;
  Grid := frameRasters.Grid;
  for RasterIndex := 0 to Rasters.Count - 1 do
  begin
    RasterItem := Rasters[RasterIndex];
    Grid.Cells[Ord(rcRasterName), RasterIndex+1] := RasterItem.RasterName;
    Grid.Cells[Ord(rcFileName), RasterIndex+1] := RasterItem.FileName;
    Grid.ItemIndex[Ord(rcType), RasterIndex+1] := Ord(RasterItem.RasterType);
  end;
end;

{ TUndoLinkedRasters }

constructor TUndoLinkedRasters.Create(var Rasters: TLinkedRasterCollection);
var
  Dummy: TNotifyEvent;
begin
  Dummy := nil;
  FOldRasters := TLinkedRasterCollection.Create(Dummy);
  FOldRasters.Assign(frmGoPhast.PhastModel.LinkedRasters);
  FNewRasters := Rasters;
  Rasters := nil;
end;

function TUndoLinkedRasters.Description: string;
begin
  result := StrChangeLinkedRaster
end;

destructor TUndoLinkedRasters.Destroy;
begin
  FOldRasters.Free;
  FNewRasters.Free;
  inherited;
end;

procedure TUndoLinkedRasters.DoCommand;
begin
  frmGoPhast.PhastModel.LinkedRasters := FNewRasters;
  inherited;
end;

procedure TUndoLinkedRasters.Undo;
begin
  frmGoPhast.PhastModel.LinkedRasters := FOldRasters;
  inherited;
end;

end.
