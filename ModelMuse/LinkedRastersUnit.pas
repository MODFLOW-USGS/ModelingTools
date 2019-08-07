unit LinkedRastersUnit;

interface

uses
  GoPhastTypes, System.Classes, RasterValuesAlongSegmentsUnit, System.SysUtils;

type
  TRasterType = (rtSurferGrid, rtEsriAscii);

  TLinkedRasterItem = class(TPhastCollectionItem)
  private
    FFileName: string;
    FRasterType: TRasterType;
    FRasterName: string;
    FRaster: IRaster;
    FRasterFileDate: TDateTime;
    procedure SetFileName(const Value: string);
    procedure SetRasterName(const Value: string);
    procedure SetRasterType(const Value: TRasterType);
    function GetRaster: IRaster;
    function GetRelativeFileName: string;
    procedure SetRelativeFileName(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    property Raster: IRaster read GetRaster;
    property FileName: string read FFileName write SetFileName;
    destructor Destroy; override;
  published
    property RasterName: string read FRasterName write SetRasterName;
    property RasterType: TRasterType read FRasterType write SetRasterType;
    property RelativeFileName: string read GetRelativeFileName
      write SetRelativeFileName;
  end;

  TLinkedRasterCollection = class(TPhastCollection)
  private
    FLastItem: TLinkedRasterItem;
    function GetItem(Index: Integer): TLinkedRasterItem;
    procedure SetItem(Index: Integer; const Value: TLinkedRasterItem);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    function Add: TLinkedRasterItem;
    property Items[Index: Integer]: TLinkedRasterItem read GetItem
      write SetItem; default;
    function RasterByName(RasterName: string): IRaster;
  end;

implementation

uses
  System.IOUtils, AsciiRasterReaderUnit, SurferGridFileReaderUnit,
  frmGoPhastUnit, frmProgressUnit;

resourcestring
  StrReadingS = 'Reading %s';

{ TLinkedRasterItem }

procedure TLinkedRasterItem.Assign(Source: TPersistent);
var
  SourceItem: TLinkedRasterItem;
begin
  if Source is TLinkedRasterItem then
  begin
    SourceItem := TLinkedRasterItem(Source);
    RasterName := SourceItem.RasterName;
    FileName := SourceItem.FileName;
    RasterType := SourceItem.RasterType;
  end
  else
  begin
    inherited;
  end;
end;

destructor TLinkedRasterItem.Destroy;
var
  ParentCollection: TLinkedRasterCollection;
begin
  ParentCollection := Collection as TLinkedRasterCollection;
  if ParentCollection.FLastItem = self then
  begin
    ParentCollection.FLastItem := nil;
  end;
  inherited;
end;

function TLinkedRasterItem.GetRaster: IRaster;
var
  Surfer6Raster: TSurferRaster6;
  AsciRaster: TAsciiRaster;
  AsciiRasterReader: TAsciiRasterReader;
  Surfer7Raster: TSurferRaster7;
  ProgressForm: TfrmProgressMM;
begin
  if Assigned(FRaster) then
  begin
    if not FileExists(FileName) then
    begin
      FRaster := nil;
    end
    else if FRasterFileDate <> TFile.GetLastWriteTime(FileName) then
    begin
      FRaster := nil;
    end;
  end;
  if not Assigned(FRaster) then
  begin
    if FileExists(FileName) then
    begin
      ProgressForm := TfrmProgressMM.Create(nil);
      try
        ProgressForm.Prefix := Format(StrReadingS, [FileName]);
        ProgressForm.Show;
        ProgressForm.BringToFront;
        case RasterType of
          rtSurferGrid:
            begin
              case SurferFileType(FileName) of
                sft6:
                  begin
                    Surfer6Raster := TSurferRaster6.Create;
                    ReadSurfer6GrdFile(FileName, Surfer6Raster);
                    FRaster := Surfer6Raster;
                    FRasterFileDate := TFile.GetLastWriteTime(FileName);
                  end;
                sft7:
                  begin
                    Surfer7Raster := TSurferRaster7.Create;
                    ReadSurfer7GrdFile(FileName, Surfer7Raster);
                    FRaster := Surfer7Raster;
                    FRasterFileDate := TFile.GetLastWriteTime(FileName);
                  end;
                sftAscii:
                  begin
                    Surfer6Raster := TSurferRaster6.Create;
                    ReadSurferAsciiFile(FileName, Surfer6Raster);
                    FRaster := Surfer6Raster;
                    FRasterFileDate := TFile.GetLastWriteTime(FileName);
                  end;
                else
                  Assert(False);
              end;
            end;
          rtEsriAscii:
            begin
              AsciiRasterReader := TAsciiRasterReader.Create;
              try
                AsciiRasterReader.FileName := FileName;
                AsciRaster := TAsciiRaster.Create;
                AsciiRasterReader.ReadAsciiRaster(AsciRaster, ProgressForm.pbProgress);
                FRaster := AsciRaster;
                FRasterFileDate := TFile.GetLastWriteTime(FileName);
              finally
                AsciiRasterReader.Free;
              end;
            end;
          else
            Assert(False);
        end;
      finally
        ProgressForm.Free;
      end;
    end;
  end;
  result := FRaster;
end;

function TLinkedRasterItem.GetRelativeFileName: string;
begin
  result := ExtractRelativePath(frmGoPhast.PhastModel.ModelFileName, FileName);
end;

procedure TLinkedRasterItem.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    FRaster := nil;
  end;
  SetStringProperty(FFileName, Value);
end;

procedure TLinkedRasterItem.SetRasterName(const Value: string);
begin
  SetStringProperty(FRasterName, Value);
end;

procedure TLinkedRasterItem.SetRasterType(const Value: TRasterType);
begin
  if FRasterType <> Value then
  begin
    FRaster := nil;
    FRasterType := Value;
    InvalidateModel;
  end;
end;

procedure TLinkedRasterItem.SetRelativeFileName(const Value: string);
var
  CurrentDir: string;
begin
  CurrentDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFileDir(frmGoPhast.PhastModel.ModelFileName));
    FileName := ExpandFileName(Value);
  finally
    SetCurrentDir(CurrentDir)
  end;
end;

{ TLinkedRasterCollection }

function TLinkedRasterCollection.Add: TLinkedRasterItem;
begin
  Result := inherited Add as TLinkedRasterItem
end;

constructor TLinkedRasterCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TLinkedRasterItem, InvalidateModelEvent);
end;

function TLinkedRasterCollection.GetItem(Index: Integer): TLinkedRasterItem;
begin
  result := inherited Items[Index] as TLinkedRasterItem;
end;

function TLinkedRasterCollection.RasterByName(RasterName: string): IRaster;
var
  index: Integer;
begin
  Result := nil;
  RasterName := UpperCase(RasterName);
  if Assigned(FLastItem) and (RasterName = UpperCase(FLastItem.RasterName)) then
  begin
    result := FLastItem.Raster;
    Exit;
  end;
  for index := 0 to Count - 1 do
  begin
    if RasterName = UpperCase(Items[index].RasterName) then
    begin
      result := Items[index].Raster;
      FLastItem :=  Items[index];
      break;
    end;
  end;
end;

procedure TLinkedRasterCollection.SetItem(Index: Integer;
  const Value: TLinkedRasterItem);
begin
  inherited Items[Index] := Value;
end;

end.
