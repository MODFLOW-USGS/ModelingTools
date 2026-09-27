unit frmDisvQualityMetricsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4,
  ModflowIrregularMeshUnit;

type
  TfrmDisvQualityMetrics = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    pcMain: TPageControl;
    pnlOverall: TPanel;
    lblMinAngle: TLabel;
    lblMinMidFraction: TLabel;
    tabCrossingAngles: TTabSheet;
    tabCrossingFractions: TTabSheet;
    pbCrossingFractions: TPaintBox;
    rdgCrossingFractions: TRbwDataGrid4;
    splCrossingFractions: TSplitter;
    splCrossingAngles: TSplitter;
    pbCrossingAngles: TPaintBox;
    rdgCrossingAngles: TRbwDataGrid4;
    procedure FormDestroy(Sender: TObject); override;
    procedure FormCreate(Sender: TObject); override;
    procedure FormShow(Sender: TObject);
    procedure pbCrossingAnglesPaint(Sender: TObject);
    procedure pbCrossingFractionsPaint(Sender: TObject);
    procedure rdgCrossingAnglesButtonClick(Sender: TObject; ACol, ARow: LongInt);
  private
    AngleList: TMeshQualityRecordList;
    MidpointList: TMeshQualityRecordList;
    { Private declarations }
  public
    procedure GetData;
    { Public declarations }
  end;

  TQualityColumns = (qcCell1, qcCell2, qcMetric);

var
  frmDisvQualityMetrics: TfrmDisvQualityMetrics;

implementation

uses
  frmGoPhastUnit, System.Math, xygraph, System.Generics.Collections,
  System.Generics.Defaults, GoPhastTypes, frmGoToUnit;

{$R *.dfm}

procedure TfrmDisvQualityMetrics.FormDestroy(Sender: TObject);
begin
  frmDisvQualityMetrics := nil;
  AngleList.Free;
  MidpointList.Free;
  inherited;
end;

procedure TfrmDisvQualityMetrics.FormCreate(Sender: TObject);
begin
  inherited;
  AngleList := TMeshQualityRecordList.Create;
  MidpointList := TMeshQualityRecordList.Create;
end;

procedure TfrmDisvQualityMetrics.FormShow(Sender: TObject);
begin
  inherited;
  pcMain.ActivePageIndex := 0;
end;


{ TfrmDisvQualityMetrics }

procedure TfrmDisvQualityMetrics.GetData;
var
  DisvMesh: TModflowDisvGrid;
  Quality: TMeshQualityRecord;
begin
  Assert(frmGoPhast.DisvUsed);
  DisvMesh := frmGoPhast.PhastModel.DisvGrid;
  Assert(DisvMesh.MeshType in [mtUnknown, mtPolygon]);
  DisvMesh.TwoDGrid.GetMeshQualityMetrics(AngleList);
  MidpointList.Clear;
  MidpointList.AddRange(AngleList);
  AngleList.Sort(TComparer<TMeshQualityRecord>.Construct(
    function (const L, R: TMeshQualityRecord): integer
    begin
      result := Sign(L.Angle - R.Angle);
    end)
    );

  MidpointList.Sort(TComparer<TMeshQualityRecord>.Construct(
    function (const L, R: TMeshQualityRecord): integer
    begin
      result := Sign(L.MidpointFraction - R.MidpointFraction);
    end)
    );

  lblMinAngle.Caption := Format('Minimum Crossing  Angle = %g', [AngleList.First.Angle]);
  lblMinMidFraction.Caption := Format('Minimum Crossing Fraction = %g', [AngleList.First.MidpointFraction]);

  rdgCrossingAngles.BeginUpdate;
  try
    rdgCrossingAngles.RowCount := AngleList.Count + 1;
    rdgCrossingAngles.Cells[Ord(qcCell1), 0] := 'Cell 1';
    rdgCrossingAngles.Cells[Ord(qcCell2), 0] := 'Cell 2';
    rdgCrossingAngles.Cells[Ord(qcMetric), 0] := 'Angle';
    for var Index := 0 to AngleList.Count - 1 do
    begin
      Quality := AngleList[Index];
      rdgCrossingAngles.Cells[Ord(qcCell1), Index+1] := Quality.Cell1.DisplayNumber.ToString;
      rdgCrossingAngles.Cells[Ord(qcCell2), Index+1] := Quality.Cell2.DisplayNumber.ToString;
      rdgCrossingAngles.Cells[Ord(qcMetric), Index+1] := Quality.Angle.ToString;
    end;
  finally
    rdgCrossingAngles.EndUpdate;
  end;

  rdgCrossingFractions.BeginUpdate;
  try
    rdgCrossingFractions.RowCount := MidpointList.Count + 1;
    rdgCrossingFractions.Cells[Ord(qcCell1), 0] := 'Cell 1';
    rdgCrossingFractions.Cells[Ord(qcCell2), 0] := 'Cell 2';
    rdgCrossingFractions.Cells[Ord(qcMetric), 0] := 'Fractions';
    for var Index := 0 to MidpointList.Count - 1 do
    begin
      Quality := MidpointList[Index];
      rdgCrossingFractions.Cells[Ord(qcCell1), Index+1] := Quality.Cell1.DisplayNumber.ToString;
      rdgCrossingFractions.Cells[Ord(qcCell2), Index+1] := Quality.Cell2.DisplayNumber.ToString;
      rdgCrossingFractions.Cells[Ord(qcMetric), Index+1] := Quality.MidpointFraction.ToString;
    end;
  finally
    rdgCrossingFractions.EndUpdate;
  end;

end;

procedure TfrmDisvQualityMetrics.pbCrossingAnglesPaint(Sender: TObject);
var
  XMin: double;
begin
  inherited;

  if AngleList.Count = 0 then
  begin
    Exit;
  end;

  XMin := AngleList.First.Angle;
  if 90 - XMin < 0.0001 then
  begin
    Exit;
  end;

  xycleargraph(pbCrossingAngles,clwhite,clblack,8/8);
  xystartgraph(0,100,0,100,40,20,30,40,clipon);
  xysetgridlines(clred);

  xyxaxis(
          clBlack,
//            color:Tcolor;
          XMin, 90,
//            min,max:single;
          0,
//            incr:single
          0,
//            zmax:single;
          'Crossing Angle',
//            txt:string;
          1,
//            type:integer;
          False,
//            grid:boolean;
          False,
//            log:boolean;
          True,
//            fix:boolean;
         2);
//            endfit:integer);

  xyyaxis(clblack,0,1,0,0,'Cumulative Frequency',5,gridon,lin,fixed, 2);
  XYLineWidth(2) ;
  XYSetLineStyle(0,0,0);

  xyMove(AngleList.First.Angle, 0);
  for var Index := 1 to AngleList.Count - 1 do
  begin
    xydraw(AngleList[Index].Angle, Index/(AngleList.Count - 1));
  end;
end;

procedure TfrmDisvQualityMetrics.pbCrossingFractionsPaint(Sender: TObject);
var
  XMin: double;
  XMax: double;
begin
  inherited;
  if MidpointList.Count = 0 then
  begin
    Exit;
  end;

  XMin := MidpointList.First.MidpointFraction;
  XMax := MidpointList.Last.MidpointFraction;
  if XMax - XMin < 0.0001 then
  begin
    Exit;
  end;

  xycleargraph(pbCrossingFractions,clwhite,clblack,8/8);
  xystartgraph(0,100,0,100,40,20,30,40,clipon);
  xysetgridlines(clred);

  xyxaxis(
          clBlack,
//            color:Tcolor;
          XMin, XMax,
//            min,max:single;
          0,
//            incr:single
          0,
//            zmax:single;
          'Crossing Fraction',
//            txt:string;
          1,
//            type:integer;
          False,
//            grid:boolean;
          False,
//            log:boolean;
          True,
//            fix:boolean;
         2);
//            endfit:integer);

  xyyaxis(clblack,0,1,0,0,'Cumulative Frequency',5,gridon,lin,fixed, 2);
  XYLineWidth(2) ;
  XYSetLineStyle(0,0,0);

  xyMove(MidpointList.First.MidpointFraction, 0);
  for var Index := 1 to MidpointList.Count - 1 do
  begin
    xydraw(MidpointList[Index].MidpointFraction, Index/(MidpointList.Count - 1));
  end;
end;

procedure TfrmDisvQualityMetrics.rdgCrossingAnglesButtonClick(Sender: TObject;
    ACol, ARow: LongInt);
var
  Grid: TRbwDataGrid4;
  ElementNumber: Integer;
begin
  inherited;
  Grid := Sender as TRbwDataGrid4;
  ElementNumber := Grid.IntegerValue[ACol, ARow] -1;
  MoveToDisv(ElementNumber, 0, [vdTop, vdFront]);
end;

end.
