unit PestPropertiesUnit;

interface

uses
  System.Classes, GoPhastTypes, GR32;

type
  TPestProperties = class(TGoPhastPersistent)
  private
    FTemplateCharacter: Char;
    FExtendedTemplateCharacter: Char;
    FPestUsed: Boolean;
    FShowPilotPoints: Boolean;
    FStoredPilotPointSpacing: TRealStorage;
    procedure SetTemplateCharacter(const Value: Char);
    procedure SetExtendedTemplateCharacter(const Value: Char);
    function GetPilotPointSpacing: double;
    procedure SetPilotPointSpacing(const Value: double);
    procedure SetPestUsed(const Value: Boolean);
    procedure SetShowPilotPoints(const Value: Boolean);
    procedure SetStoredPilotPointSpacing(const Value: TRealStorage);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    procedure InitializeVariables;
    property PilotPointSpacing: double read GetPilotPointSpacing
      write SetPilotPointSpacing;
    procedure DrawPilotPoints(BitMap32: TBitmap32);
    function ShouldDrawPilotPoints: Boolean;
  Published
    property PestUsed: Boolean read FPestUsed write SetPestUsed Stored True;
    property TemplateCharacter: Char read FTemplateCharacter
      write SetTemplateCharacter;
    property ExtendedTemplateCharacter: Char read FExtendedTemplateCharacter
      write SetExtendedTemplateCharacter;
    property ShowPilotPoints: Boolean read FShowPilotPoints
      write SetShowPilotPoints Stored True;
    property StoredPilotPointSpacing: TRealStorage
      read FStoredPilotPointSpacing write SetStoredPilotPointSpacing;
  end;

implementation

uses
  ZoomBox2, BigCanvasMethods, frmGoPhastUnit;

{ TPestProperties }

procedure TPestProperties.Assign(Source: TPersistent);
var
  PestSource: TPestProperties;
begin
  if Source is TPestProperties then
  begin
    PestSource := TPestProperties(Source);
    PestUsed := PestSource.PestUsed;
    ShowPilotPoints := PestSource.ShowPilotPoints;
    PilotPointSpacing := PestSource.PilotPointSpacing;
    TemplateCharacter := PestSource.TemplateCharacter;
    ExtendedTemplateCharacter := PestSource.ExtendedTemplateCharacter;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPestProperties.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;

  FStoredPilotPointSpacing := TRealStorage.Create;

  InitializeVariables;

  FStoredPilotPointSpacing.OnChange := InvalidateModelEvent;
end;

destructor TPestProperties.Destroy;
begin
  FStoredPilotPointSpacing.Free;

  inherited;
end;

procedure TPestProperties.DrawPilotPoints(BitMap32: TBitmap32);
var
  ZoomBox: TQRbwZoomBox2;
  DisLimits: TGridLimit;
  RowCount: Int64;
  ColumnCount: Int64;
  RowIndex: Integer;
  LeftX: double;
  TopY: double;
  Y: Double;
  YInt: Integer;
  X: Double;
  LineSegment: GoPhastTypes.TPointArray;
  ColIndex: Integer;
  XInt: Integer;
begin
  if ShouldDrawPilotPoints then
  begin
    SetLength(LineSegment, 2);
    ZoomBox := frmGoPhast.frameTopView.ZoomBox;
    DisLimits := frmGoPhast.PhastModel.DiscretizationLimits(vdTop);
    RowCount := Trunc((DisLimits.MaxY - DisLimits.MinY)/PilotPointSpacing) + 1;
    ColumnCount := Trunc((DisLimits.MaxX - DisLimits.MinX)/PilotPointSpacing) + 1;
    LeftX := (DisLimits.MaxX + DisLimits.MinX)/2 - ColumnCount/2*PilotPointSpacing;
    TopY := (DisLimits.MaxY + DisLimits.MinY)/2 + RowCount/2*PilotPointSpacing;
    for RowIndex := 0 to RowCount do
    begin
      Y := TopY - RowIndex*PilotPointSpacing;
      YInt := ZoomBox.YCoord(Y);
      for ColIndex := 0 to ColumnCount do
      begin
        X := LeftX + ColIndex*PilotPointSpacing;
        XInt := ZoomBox.XCoord(X);

        LineSegment[0].x := XInt;
        LineSegment[1].x := XInt;
        LineSegment[0].y := YInt-2;
        LineSegment[1].y := YInt+3;
        DrawBigPolyline32(BitMap32, clBlack32, 1, LineSegment,
          True, False, 0, 2);

        LineSegment[0].x := XInt-2;
        LineSegment[1].x := XInt+3;
        LineSegment[0].y := YInt;
        LineSegment[1].y := YInt;
        DrawBigPolyline32(BitMap32, clBlack32, 1, LineSegment,
          True, False, 0, 2);
      end;
    end;
  end;
end;

function TPestProperties.ShouldDrawPilotPoints: Boolean;
begin
  result := PestUsed and ShowPilotPoints and (PilotPointSpacing > 0);
end;

function TPestProperties.GetPilotPointSpacing: double;
begin
  result := FStoredPilotPointSpacing.Value;
end;

procedure TPestProperties.InitializeVariables;
begin
  {$IFDEF PEST}
  // When PEST is removed, the default should be FPestUsed := False;
  FPestUsed := True;
  {$ELSE}
  FPestUsed := False;
  {$ENDIF}
  FShowPilotPoints := False;
  PilotPointSpacing := 0;
  FTemplateCharacter := '@';
  FExtendedTemplateCharacter := '%';
end;

procedure TPestProperties.SetExtendedTemplateCharacter(const Value: Char);
begin
  SetCharacterProperty(FExtendedTemplateCharacter, Value);
end;

procedure TPestProperties.SetPestUsed(const Value: Boolean);
begin
  SetBooleanProperty(FPestUsed, Value);
end;

procedure TPestProperties.SetPilotPointSpacing(const Value: double);
begin
  FStoredPilotPointSpacing.Value := Value;
end;

procedure TPestProperties.SetShowPilotPoints(const Value: Boolean);
begin
  FShowPilotPoints := Value;
end;

procedure TPestProperties.SetStoredPilotPointSpacing(const Value: TRealStorage);
begin
  FStoredPilotPointSpacing.Assign(Value);
end;

procedure TPestProperties.SetTemplateCharacter(const Value: Char);
begin
  SetCharacterProperty(FTemplateCharacter, Value);
end;

end.
