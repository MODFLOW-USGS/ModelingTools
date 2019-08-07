unit frmGeoRefUnit;

interface

uses System.UITypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, GeoRefUnit, UndoItems;

type
  TfrmGeoRef = class(TfrmCustomGoPhast)
    rrdgGeoRef: TRbwRowDataGrid;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure rrdgGeoRefSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormDestroy(Sender: TObject); override;
    procedure FormShow(Sender: TObject);
  private
    FGeoRef: TGeoRef;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoGeoRef = class(TCustomUndo)
  private
    FNewGeoRef: TGeoRef;
    FOldGeoRef: TGeoRef;
  protected
    function Description: string; override;
  public
    constructor Create(var GeoRef: TGeoRef);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;

  end;

var
  frmGeoRef: TfrmGeoRef;

implementation

uses
  frmGoPhastUnit, GoPhastTypes;


{$R *.dfm}

type
  TGeoRefCol = (grcCaption, grcValue);
  TGeoRefRow = (grrCaption, grrUpperLeftX, grrUpperLeftY, grrRotation, grrLengthUnit,
    grrTimeUnit, grrStartDate, grrStartTime, grrModelType, grrProjectionType,
    grrProjection);

procedure TfrmGeoRef.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmGeoRef.FormCreate(Sender: TObject);
begin
  inherited;

  rrdgGeoRef.BeginUpdate;
  try
    rrdgGeoRef.Cells[Ord(grcValue), Ord(grrCaption)] := 'Value';

    rrdgGeoRef.Cells[Ord(grcCaption), Ord(grrUpperLeftX)] := 'Upper left X';
    rrdgGeoRef.Cells[Ord(grcCaption), Ord(grrUpperLeftY)] := 'Upper left Y';
    rrdgGeoRef.Cells[Ord(grcCaption), Ord(grrRotation)] := 'Grid rotation angle';
    rrdgGeoRef.Cells[Ord(grcCaption), Ord(grrLengthUnit)] := 'Length unit';
    rrdgGeoRef.Cells[Ord(grcCaption), Ord(grrTimeUnit)] := 'Time unit';
    rrdgGeoRef.Cells[Ord(grcCaption), Ord(grrStartDate)] := 'Start date (mm/dd/yyyy)';
    rrdgGeoRef.Cells[Ord(grcCaption), Ord(grrStartTime)] := 'Start time (hh:mm:ss)';
    rrdgGeoRef.Cells[Ord(grcCaption), Ord(grrModelType)] := 'Model type';
    rrdgGeoRef.Cells[Ord(grcCaption), Ord(grrProjectionType)] := 'Projection type';
    rrdgGeoRef.Cells[Ord(grcCaption), Ord(grrProjection)] := 'Projection';

    GetData;
  finally
    rrdgGeoRef.EndUpdate
  end;
//xul 1157053.959            # x-coordinate of the upper left of the model grid
//yul 405727.084             # y-coordinate of the upper left of the model grid
//rotation -45.957964853112  # rotation of the model grid in degrees
//length_units feet          # length unit of the model grid (feet, meters, etc.)
//time_units days            # time unit of the model grid (days, years, etc.)
//start_date 1/1/1900        # state date of the model
//start_time 00:00:00        # start time of the model
//model modflow-2000         # MODFLOW model type (MODFLOW-NWT, etc.)
//epsg 102733                # or proj4 string instead of epsg code
//# proj4 'proj4 string'     # or epsg code instead of proj4 string

end;

procedure TfrmGeoRef.FormDestroy(Sender: TObject);
begin
  inherited;
  FGeoRef.Free;
end;

procedure TfrmGeoRef.FormShow(Sender: TObject);
var
  RowIndex: Integer;
  ARect: TGridRect;
  CanSelect: Boolean;
begin
  inherited;
  for RowIndex := 1 to rrdgGeoRef.RowCount - 1 do
  begin
    CanSelect := True;
    rrdgGeoRefSelectCell(rrdgGeoRef, 1, RowIndex, CanSelect);
    if CanSelect then
    begin
      ARect.Left := 1;
      ARect.Right := 1;
      ARect.Top := RowIndex;
      ARect.Bottom := RowIndex;
      rrdgGeoRef.Selection := ARect;
      Break;
    end;
  end;
end;

procedure TfrmGeoRef.GetData;
var
  LengthPickList: TStrings;
  TimePickList: TStrings;
//  Year: Word;
//  Month: Word;
//  Day: Word;
  Hour, Min, Sec, MSec: Word;
  ModelType: TModelSelection;
begin
  FGeoRef := TGeoRef.Create(nil);
  FGeoRef.Assign(frmGoPhast.PhastModel.GeoRef);

  LengthPickList := rrdgGeoRef.Rows[Ord(grrLengthUnit)].PickList;
  TimePickList := rrdgGeoRef.Rows[Ord(grrTimeUnit)].PickList;
  LengthPickList.Clear;
  TimePickList.Clear;
  ModelType := FGeoRef.Modeltype;
  AssignLengthUnitStringsToPicklist(LengthPickList, ModelType);
  AssignTimeUnitStringsToPicklist(TimePickList, ModelType);
  case FGeoRef.Modeltype of
    msPhast:
      begin
        rrdgGeoRef.Rows[Ord(grrLengthUnit)].LimitToList := True;
        rrdgGeoRef.Rows[Ord(grrTimeUnit)].LimitToList := True;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT, msModflowFmp,
      msModflowCfp, msModflow2015:
      begin
        rrdgGeoRef.Rows[Ord(grrLengthUnit)].LimitToList := True;
        rrdgGeoRef.Rows[Ord(grrTimeUnit)].LimitToList := True;
      end;
    msSutra22, msSutra30, msFootPrint:
      begin
        rrdgGeoRef.Rows[Ord(grrLengthUnit)].LimitToList := False;
        rrdgGeoRef.Rows[Ord(grrTimeUnit)].LimitToList := False;
        rrdgGeoRef.Rows[Ord(grrLengthUnit)].ComboUsed := True;
        rrdgGeoRef.Rows[Ord(grrTimeUnit)].ComboUsed := True;
      end;
    else
      Assert(False);
  end;

  rrdgGeoRef.RealValue[Ord(grcValue), Ord(grrUpperLeftX)] := FGeoRef.UpperLeftX;
  rrdgGeoRef.RealValue[Ord(grcValue), Ord(grrUpperLeftY)] := FGeoRef.UpperLeftY;
  rrdgGeoRef.RealValue[Ord(grcValue), Ord(grrRotation)] := FGeoRef.Rotation*180/Pi;
  rrdgGeoRef.Cells[Ord(grcValue), Ord(grrLengthUnit)] := FGeoRef.LengthUnit;
  rrdgGeoRef.Cells[Ord(grcValue), Ord(grrTimeUnit)] := FGeoRef.TimeUnit;

//  DecodeDate(FGeoRef.StartDate, Year, Month, Day);
  rrdgGeoRef.Cells[Ord(grcValue), Ord(grrStartDate)] := DateToStr(FGeoRef.StartDate);
//    Format('%0:d/%1:d/%2:d', [Month, Day, Year]);

  DecodeTime(FGeoRef.StartTime, Hour, Min, Sec, MSec);
  rrdgGeoRef.Cells[Ord(grcValue), Ord(grrStartTime)] :=
    Format('%0:d:%1:d:%2:d', [Hour, Min, Sec]);

  rrdgGeoRef.Cells[Ord(grcValue), Ord(grrModelType)] := FGeoRef.ModelProgramName;
  if FGeoRef.ProjectionType in [ptEpsg, ptProj4] then
  begin
    rrdgGeoRef.ItemIndex[Ord(grcValue), Ord(grrProjectionType)] := Ord(FGeoRef.ProjectionType);
  end
  else
  begin
    rrdgGeoRef.ItemIndex[Ord(grcValue), Ord(grrProjectionType)] := -1;
  end;
  rrdgGeoRef.Cells[Ord(grcValue), Ord(grrProjection)] := FGeoRef.Projection;

end;

procedure TfrmGeoRef.rrdgGeoRefSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var
  GeoRefRow: TGeoRefRow;
begin
  inherited;
  if (ACol = Ord(grcValue)) and (ARow >= rrdgGeoRef.FixedRows) then
  begin
    GeoRefRow := TGeoRefRow(ARow);
    if (FGeoRef.Modeltype in [msPhast, msModflow, msModflowLGR, msModflowLGR2,
      msModflowNWT, msModflowFmp, msModflowCfp, msModflow2015])
      and (GeoRefRow in [grrLengthUnit, grrTimeUnit]) then
    begin
      CanSelect := False;
    end
    else if (GeoRefRow in [grrUpperLeftX, grrUpperLeftY, grrRotation,
      grrModelType]) then
    begin
      CanSelect := False;
    end;
  end;

end;

procedure TfrmGeoRef.SetData;
var
//  Splitter: TStringList;
//  Month, Day, Year: Integer;
  ProjectionTypeIndex: Integer;
begin
  case FGeoRef.Modeltype of
    msPhast:
      begin
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT, msModflowFmp,
      msModflowCfp, msModflow2015:
      begin
      end;
    msSutra22, msSutra30, msFootPrint:
      begin
        FGeoRef.OtherLengthUnits := rrdgGeoRef.Cells[Ord(grcValue), Ord(grrLengthUnit)];
        FGeoRef.OtherTimeUnits := rrdgGeoRef.Cells[Ord(grcValue), Ord(grrTimeUnit)];
      end;
    else
      Assert(False);
  end;
//  Splitter := TStringList.Create;
  try
    FGeoRef.StartDate := StrToDate(rrdgGeoRef.Cells[Ord(grcValue), Ord(grrStartDate)]);
//    Splitter.Delimiter := '/';
//    Splitter.DelimitedText := ;
//    if Splitter.Count = 3 then
//    begin
//      if TryStrToInt(Splitter[0], Month)
//        and TryStrToInt(Splitter[1], Day)
//        and TryStrToInt(Splitter[2], Year) then
//      begin
//        FGeoRef.StartDate := EncodeDate(Year, Month, Day)
//      end;
//    end;
//
//    Splitter.Delimiter := ':';
//    Splitter.DelimitedText := rrdgGeoRef.Cells[Ord(grcValue), Ord(grrStartTime)];
//    if Splitter.Count = 3 then
//    begin
//      if TryStrToInt(Splitter[0], Hour)
//        and TryStrToInt(Splitter[1], Minute)
//        and TryStrToInt(Splitter[2], Second) then
//      begin
//        FGeoRef.StartTime := EncodeTime(Hour, Minute, Second, 0)
//      end;
//    end;
  except on E: EConvertError do
    begin
      Beep;
      MessageDlg(E.message, mtError, [mbOK], 0);
      ModalResult := mrNone;
      Exit;
    end;
//    Splitter.Free;
  end;
  ProjectionTypeIndex := rrdgGeoRef.ItemIndex[Ord(grcValue), Ord(grrProjectionType)];
  if ProjectionTypeIndex >= 0 then
  begin
    FGeoRef.ProjectionType := TProjectionType(rrdgGeoRef.ItemIndex[Ord(grcValue), Ord(grrProjectionType)]);
  end;
  FGeoRef.Projection := rrdgGeoRef.Cells[Ord(grcValue), Ord(grrProjection)];

  frmGoPhast.UndoStack.Submit(TUndoGeoRef.Create(FGeoRef));

end;

{ TUndoGeoRef }

constructor TUndoGeoRef.Create(var GeoRef: TGeoRef);
begin
  Inherited Create;
  FNewGeoRef := GeoRef;
  GeoRef := nil;
  FOldGeoRef := TGeoRef.Create(nil);
  FOldGeoRef.Assign(frmGoPhast.PhastModel.GeoRef);
end;

function TUndoGeoRef.Description: string;
begin
  result := 'edit Geo Ref'
end;

destructor TUndoGeoRef.Destroy;
begin
  FNewGeoRef.Free;
  FOldGeoRef.Free;
  inherited;
end;

procedure TUndoGeoRef.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.GeoRef := FNewGeoRef;
end;

procedure TUndoGeoRef.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.GeoRef := FOldGeoRef;
end;

end.
