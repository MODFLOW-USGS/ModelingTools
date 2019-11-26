Compiling GW_Chart requires making some changes to the source code of the TChart (version 6.01) component from Steema Software (http://www.steema.com).  The required changes are as follows.

In the interface section of TeEngine.pas add a new type after the declaration of TSeriesClick.  The new type is as follows.

  TOnDrawAxisLabel = procedure( Sender: TChartAxis; X, Y, Z: integer;
                                AxisLabel: string; var DrawLabel: boolean) of object;

In the interface section of TeEngine.pas in the private section of TCustomAxisPanel add the following.

    FOnDrawAxisLabel: TOnDrawAxisLabel;

In the interface section of TeEngine.pas in the protected section of TCustomAxisPanel add the following.

    property OnDrawAxisLabel: TOnDrawAxisLabel read FOnDrawAxisLabel write FOnDrawAxisLabel;

In the implementation section of TeEngine.pas in the implementation of procedure TChartAxis.DrawAxisLabel add the following new local variable at the end of the list of local variables.

    DrawLabel: boolean;

In the implementation section of TeEngine.pas in the implementation of procedure TChartAxis.DrawAxisLabel after the line "y:=y+Delta" replace 
        if FLabelsExponent then DrawExponentLabel
                           else ParentChart.Canvas.TextOut3D(X,Y,tmpZ,tmpSt2);

with 
      DrawLabel := True;
      if Assigned(ParentChart.OnDrawAxisLabel) then
      begin
        ParentChart.OnDrawAxisLabel( self, X, Y, tmpZ, tmpSt2, DrawLabel);
      end;
      if DrawLabel then
      begin
        if FLabelsExponent then DrawExponentLabel
                           else ParentChart.Canvas.TextOut3D(X,Y,tmpZ,tmpSt2);
      end

In the implementation section of TeEngine.pas in the implementation of procedure TChartAxis.DrawAxisLabel after the line "if (Angle=90) or (Angle=270) then x:=x+Delta;" replace 

        ParentChart.Canvas.RotateLabel3D(X,Y,tmpZ,tmpSt2,Angle);

with 
      DrawLabel := True;
      if Assigned(ParentChart.OnDrawAxisLabel) then
      begin
        ParentChart.OnDrawAxisLabel( self, X, Y, tmpZ, tmpSt2, DrawLabel);
      end;
      if DrawLabel then
      begin
        ParentChart.Canvas.RotateLabel3D(X,Y,tmpZ,tmpSt2,Angle);
      end

In the implementation section of TeEngine.pas change the body of Function TChartValueLists.Get from 

    result:=List^[Index];

to 

  if Index < 0 then
  begin
    result := nil;
  end
  else
  begin
    result:=List^[Index];
  end;


In the interface section of Chart.pas add the following to the published section of TChart.

    property OnDrawAxisLabel;

In the implementation section of TeEngine.pas in the implementation of procedure TSeriesPointer.PrepareCanvas

in the next to the last line replace
  ACanvas.AssignBrushColor(Brush,tmp,ColorValue);
with
  ACanvas.AssignBrushColor(Brush,ColorValue,tmp);

In TeeEditCha.pas change all instances of '.hlp' to '.chm'.

In TEngine.pas, in TSeriesPointer.DrawPointer, the code for drawing a cirle should be changed from 

                       EllipseWithZ(PXMinus,PYMinus,PXPlus,PYPlus,GetStartZ)
to
                       EllipseWithZ(PXMinus,PYMinus,PXPlus,PYPlus,GetMiddleZ)

In TEngine.pas, in TSeriesPointer.DrawPointer, the following lines in DrawDiagonalCross should be changed:
from
        LineWithZ(PXMinus, PYMinus, PXPlus+1,PYPlus+1,GetStartZ);
        LineWithZ(PXPlus,  PYMinus, PXMinus-1,PYPlus+1,GetStartZ);
to
        LineWithZ(PXMinus, PYMinus, PXPlus+1,PYPlus+1,GetMiddleZ);
        LineWithZ(PXPlus,  PYMinus, PXMinus-1,PYPlus+1,GetMiddleZ);

In TEngine.pas, in TSeriesPointer.DrawPointer, the following lines in DrawCross should be changed:
from
        VertLine3D(PX,PYMinus,PYPlus+1,GetStartZ);
        HorizLine3D(PXMinus,PXPlus+1,PY,GetStartZ);
to
        VertLine3D(PX,PYMinus,PYPlus+1,GetMiddleZ);
        HorizLine3D(PXMinus,PXPlus+1,PY,GetMiddleZ);
		
In BubbleCh.pas, in the implementation of TBubbleSeries.DrawLegendShape, change the implementation to 

var tmp : Integer;
// rbw begin change
  AChart: TCustomChart;
  LocalRect: TRect;
// rbw end change
begin
  LocalRect := Rect;
  With Rect do tmp:=Math.Min((Right-Left),(Bottom-Top));
// rbw begin change
  tmp := tmp div 3;
  Dec(LocalRect.Top, tmp);
  Dec(LocalRect.Bottom, tmp);
  if Assigned(ParentChart) and (ParentChart is TCustomChart) then
  begin
    AChart := TCustomChart(ParentChart);
    if Assigned(AChart.OnGetSymbolSize) then
    begin
      AChart.OnGetSymbolSize(AChart, ValueIndex, tmp);
    end;
  end;
// rbw end change
  With TPointerAccess(Pointer) do
  begin
    ChangeHorizSize(tmp);
    ChangeVertSize(tmp);
  end;
// rbw begin change
//  inherited;
  inherited DrawLegendShape(ValueIndex, LocalRect);
// rbw end change
end;

In Chart.pas, add 
  TChartGetSymbolSizeEvent = procedure (Sender: TCustomChart; const ValueIndex: integer; var SymbolSize: integer) of object;
 after 
   TChartAllowScrollEvent=Procedure( Sender:TChartAxis; Var AMin,AMax:Double;
				    Var AllowScroll:Boolean ) of object;
					
In Chart.pas, in the declaration of TCustomChart, add a new private field
    FOnGetSymbolSize   : TChartGetSymbolSizeEvent;
and a new public property
    property OnGetSymbolSize:TChartGetSymbolSizeEvent read FOnGetSymbolSize write FOnGetSymbolSize;

In Chart.pas in the declaration of TChart add a new published properties
    property OnDrawAxisLabel;
    property OnGetSymbolSize;
	
In Chart.pas, Change the implmentation of TCustomChartLegend.CalcItemHeight to
// rbw begin change
var
  LocalChart: TCustomChart;
  SymbolSize: integer;
  ValueIndex: integer;
  ASeries: TChartSeries;
  tmp: integer;
  Changed: boolean;
// rbw end change
begin
  result:=ParentChart.Canvas.FontHeight;

  if HasCheckBoxes then
     result:=Math.Max(6+TeeCheckBoxSize,result);

// rbw begin change
  if ParentChart is TCustomChart then
  begin
    LocalChart := TCustomChart(ParentChart);
    if Assigned(LocalChart.OnGetSymbolSize) then
    begin
      ASeries := GetLegendSeries;
      SymbolSize := (result-4) div 3;
      tmp := SymbolSize;
      Changed := False;
      for ValueIndex := 0 to ASeries.Count do
      begin
        LocalChart.OnGetSymbolSize(LocalChart, ValueIndex, tmp);
        if tmp > SymbolSize then
        begin
          SymbolSize := tmp;
          Changed := True;
        end;
      end;
      if Changed then
      begin
        result := SymbolSize*3;
      end;
    end;
  end;
// rbw end change


  Inc(result,FVertSpacing);

  if Vertical and DividingLines.Visible then { 5.02 }
     Inc(result,DividingLines.Width);
end;

In TEngine.pas, in TSeriesPointer.DrawPointer, in DrawDiagonalCross, change
        LineWithZ(PXMinus, PYMinus, PXPlus+1,PYPlus+1,GetStartZ);
        LineWithZ(PXPlus,  PYMinus, PXMinus-1,PYPlus+1,GetStartZ);
to		
        LineWithZ(PXMinus, PYMinus, PXPlus+1,PYPlus+1,GetMiddleZ);
        LineWithZ(PXPlus,  PYMinus, PXMinus-1,PYPlus+1,GetMiddleZ);

In TEngine.pas, in TSeriesPointer.DrawPointer, in DrawCross, change
        VertLine3D(PX,PYMinus,PYPlus+1,GetStartZ);
        HorizLine3D(PXMinus,PXPlus+1,PY,GetStartZ);
to
        VertLine3D(PX,PYMinus,PYPlus+1,GetMiddleZ);
        HorizLine3D(PXMinus,PXPlus+1,PY,GetMiddleZ);
		
In TEngine.pas, in TSeriesPointer.DrawPointer, change 
       psCircle: if Is3D then
                    if Self.FDraw3D and SupportsFullRotation then
                       Sphere(PX,PY,GetMiddleZ,tmpHoriz)
                    else
                       EllipseWithZ(PXMinus,PYMinus,PXPlus,PYPlus,GetStartZ)
                 else
                    Ellipse(PXMinus,PYMinus,PXPlus,PYPlus);
to 
       psCircle: if Is3D then
                    if Self.FDraw3D and SupportsFullRotation then
                       Sphere(PX,PY,GetMiddleZ,tmpHoriz)
                    else
                      // RBW begin change
//                       EllipseWithZ(PXMinus,PYMinus,PXPlus,PYPlus,GetStartZ)
                       EllipseWithZ(PXMinus,PYMinus,PXPlus,PYPlus,GetMiddleZ)
                      // RBW end change
                 else
                    Ellipse(PXMinus,PYMinus,PXPlus,PYPlus);
		

 

  

