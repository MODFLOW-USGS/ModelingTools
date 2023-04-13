{@abstract(The main purpose of @name is to write the transport data input file
  for PHAST with the procedure @link(WritePhastInput).)}
unit WritePhastUnit;

interface

uses System.UITypes, Classes, GoPhastTypes;

{@name writes the transport data input file for PHAST.
 @name initializes data sets as needed and synthesizes all the data from
 the model into the input file.
 @param(FileName is the name of the transport data input file.) }
procedure WritePhastInput(const FileName: string; RunModel: boolean);

implementation

uses Windows, Dialogs, SysUtils, frmGoPhastUnit, PhastModelUnit, PrintFrequency,
  TimeUnit, InitialChemistryZone, ZoneUnit, MediaZone, ActiveZone,
  InitialHeadZone, PrintChemistryXYZ_Zone, PrintChemistryZone,
  SpecifiedHeadZone, SpecifiedFluxFrontZone, SpecifiedFluxSideZone,
  SpecifiedFluxTopZone, CustomBoundaryZone, CustomLeakyZone, FrontLeakyZone,
  SideLeakyZone, TopLeakyZone, WriteRiverUnit, ScreenObjectUnit,
  WriteWellUnit, DataSetUnit, PhastDataSets,
  ModelMuseUtilities, frmErrorsAndWarningsUnit, GeoRefWriterUnit,
  ArchiveNodeInterface, System.IOUtils, System.Types;

resourcestring
  StrNoBoundaryConditio = 'No boundary conditions specified.';
  StrYouMustSpecifyAt = 'You must specify at least one boundary condition in' +
  ' your model.';

procedure WriteTime(const Stream: TStringStream; const Time: double);
begin
  Stream.WriteString(BlankSpaces + BlankSpaces + BlankSpaces
  + FloatToStr(Time) + ' ');
end;

procedure WritePropertyName(const Stream: TStringStream; const PropertyName:
  string);
begin
  Stream.WriteString(BlankSpaces + BlankSpaces + '-' + PropertyName +
    EndOfLine);
end;

procedure WriteEnd(const Stream: TStringStream);
begin
  Stream.WriteString('END' + EndOfLine);
end;

procedure WriteTimeControl(const Stream: TStringStream;
  const Units: string);
var
  EndingTime, TimeStepLength, Time: double;
  TimeIndex: integer;
  TimeItem: TTimeItem;
begin
  Stream.WriteString('TIME_CONTROL' + EndOfLine);
  Stream.WriteString(BlankSpaces + BlankSpaces + '-start_time '
    + FloatToStr(frmGoPhast.PhastModel.Times.StartTime.Value)
    + EndOfLine);
  WritePropertyName(Stream, 'time_step');
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModelTimes.Count - 2 do
  begin
    Time := frmGoPhast.PhastModel.ModelTimes[TimeIndex];
    if TimeIndex = 0 then
    begin
      TimeItem := frmGoPhast.PhastModel.Times.Items[0] as TTimeItem;
      TimeStepLength := TimeItem.TimeStepLength;
    end
    else
    begin
      TimeStepLength := frmGoPhast.PhastModel.Times.TimeStepLength(Time);
    end;
//    Time := frmGoPhast.Model.ModelTimes[TimeIndex];
    Stream.WriteString(BlankSpaces + BlankSpaces + BlankSpaces
      + FloatToStr(Time) + ' '
      + FloatToStr(TimeStepLength) + ' ' + Units + EndOfLine);
  end;
  WritePropertyName(Stream, 'time_end');
  EndingTime := frmGoPhast.PhastModel.ModelTimes[
    frmGoPhast.PhastModel.ModelTimes.Count - 1];
  Stream.WriteString(BlankSpaces + BlankSpaces + BlankSpaces
    + FloatToStr(EndingTime) + ' ' + Units + EndOfLine);
end;

procedure WriteUnits(const Stream: TStringStream; out TimeUnits: string);
  procedure WriteLength(const Length: TLengthUnits);
  begin
    case Length of
      luInches:
        begin
          Stream.WriteString('in');
        end;
      luFeet:
        begin
          Stream.WriteString('feet');
        end;
      luMiles:
        begin
          Stream.WriteString('miles');
        end;
      luMillimeters:
        begin
          Stream.WriteString('millimeters');
        end;
      luCentimeters:
        begin
          Stream.WriteString('centimeters');
        end;
      luMeters:
        begin
          Stream.WriteString('meters');
        end;
      luKilometers:
        begin
          Stream.WriteString('kilometers');
        end;
    else
      Assert(False);
    end;
  end;
  function TimeString(const Time: TTimeUnits): string;
  begin
    case Time of
      tuSeconds:
        begin
          result := 'seconds';
        end;
      tuMinutes:
        begin
          result := 'minutes';
        end;
      tuHours:
        begin
          result := 'hours';
        end;
      tuDays:
        begin
          result := 'days';
        end;
      tuYears:
        begin
          result := 'years';
        end;
    else
      Assert(False);
    end;
  end;
  procedure WriteTime(const Time: TTimeUnits);
  begin
    Stream.WriteString(TimeString(Time));
  end;
  procedure WriteLengthItem(const Item: string;
    const Length: TLengthUnits);
  begin
    Stream.WriteString(Item + ' ');
    WriteLength(Length);
    Stream.WriteString(EndOfLine);
  end;
  procedure WriteConductivityItem(const Item: string;
    const Length: TLengthUnits; const Time: TTimeUnits);
  begin
    Stream.WriteString(Item + ' ');
    WriteLength(Length);
    Stream.WriteString('/');
    WriteTime(Time);
    Stream.WriteString(EndOfLine);
  end;
begin
  with frmGoPhast.PhastModel.Units do
  begin
    TimeUnits := TimeString(DefaultTimeUnits);
    Stream.WriteString('UNITS' + EndOfLine);
    Stream.WriteString(BlankSpaces + '-time ');
    WriteTime(DefaultTimeUnits);
    Stream.WriteString(EndOfLine);
    WriteLengthItem(BlankSpaces + '-horizontal_grid',
      DefaultHorizontalGridUnits);
    WriteLengthItem(BlankSpaces + '-vertical_grid', DefaultVerticalGridUnits);
    WriteLengthItem(BlankSpaces + '-head', DefaultHorizontalGridUnits);
    WriteConductivityItem(BlankSpaces + '-hydraulic_conductivity',
      DefaultHydraulicConductivityLengthUnits,
      DefaultHydraulicConductivityTimeUnits);
    Stream.WriteString(BlankSpaces + '-specific_storage ');
    case DefaultSpecificStorageUnits of
      iluInches:
        begin
          Stream.WriteString('1/inches');
        end;
      iluFeet:
        begin
          Stream.WriteString('1/feet');
        end;
      iluMiles:
        begin
          Stream.WriteString('1/miles');
        end;
      iluMillimeters:
        begin
          Stream.WriteString('1/millimeters');
        end;
      iluCentimeters:
        begin
          Stream.WriteString('1/centimeters');
        end;
      iluMeters:
        begin
          Stream.WriteString('1/meters');
        end;
      iluKilometers:
        begin
          Stream.WriteString('1/kilometers');
        end;
    else
      Assert(False);
    end;
    Stream.WriteString(EndOfLine);
    WriteLengthItem(BlankSpaces + '-dispersivity', DefaultDispersivityUnits);
    WriteConductivityItem(BlankSpaces + '-flux',
      DefaultFluxLengthUnits, DefaultFluxTimeUnits);
    WriteConductivityItem(BlankSpaces + '-leaky_hydraulic_conductivity',
      DefaultLeakyHydraulicConductivityLengthUnits,
      DefaultLeakyHydraulicConductivityTimeUnits);
    WriteLengthItem(BlankSpaces + '-leaky_thickness',
      DefaultLeakyThicknessUnits);
    WriteLengthItem(BlankSpaces + '-well_diameter', DefaultWellDiameterUnits);
    Stream.WriteString(BlankSpaces + '-well_flow_rate ');
    case DefaultWellFlowVolumnUnits of
      vuGallons:
        begin
          Stream.WriteString('gallons');
        end;
      vuInches3:
        begin
          Stream.WriteString('inches3');
        end;
      vuFeet3:
        begin
          Stream.WriteString('feet3');
        end;
      vuMiles3:
        begin
          Stream.WriteString('miles3');
        end;
      vuLiters:
        begin
          Stream.WriteString('liter');
        end;
      vuMillimeters3:
        begin
          Stream.WriteString('millimeters3');
        end;
      vuCentimeters3:
        begin
          Stream.WriteString('centimeters3');
        end;
      vuMeters3:
        begin
          Stream.WriteString('meters3');
        end;
      vuKilometers3:
        begin
          Stream.WriteString('kilometers3');
        end;
    else
      Assert(False);
    end;
    Stream.WriteString('/');
    WriteTime(DefaultWellFlowTimeUnits);
    Stream.WriteString(EndOfLine);
    WriteConductivityItem(BlankSpaces + '-river_bed_hydraulic_conductivity',
      DefaultRiverBedHydraulicConductivityLengthUnits,
      DefaultRiverBedHydraulicConductivityTimeUnits);
    WriteLengthItem(BlankSpaces + '-river_bed_thickness ',
      DefaultRiverBedThicknessUnits);
  end;
end;

procedure WriteSteadyFlow(const Stream: TStringStream);
begin
  with frmGoPhast.PhastModel.SteadyFlowOptions do
  begin
    if SteadyFlow then
    begin
      Stream.WriteString('STEADY_FLOW true' + EndOfLine);
      Stream.WriteString(BlankSpaces + '-head_tolerance '
        + FloatToStr(HeadTolerance) + EndOfLine);
      Stream.WriteString(BlankSpaces + '-flow_balance_tolerance '
        + FloatToStr(FlowBalanceTolerance) + EndOfLine);
      if not UseDefaultMinimumTimeStep then
      begin
        Stream.WriteString(BlankSpaces + '-minimum_time_step '
          + FloatToStr(MinimumTimeStep) + EndOfLine);
      end;
      if not UseDefaultMaximumTimeStep then
      begin
        Stream.WriteString(BlankSpaces + '-maximum_time_step '
          + FloatToStr(MaximumTimeStep) + EndOfLine);
      end;
      if not UseDefaultHeadChangeLimit then
      begin
        Stream.WriteString(BlankSpaces + '-head_change_target '
          + FloatToStr(HeadChangeLimit) + EndOfLine);
      end;
      Stream.WriteString(BlankSpaces + '-iterations '
        + IntToStr(Iterations) + EndOfLine);
    end
    else
    begin
      Stream.WriteString('STEADY_FLOW false' + EndOfLine);
    end;
  end;
end;

procedure WriteTitle(const Stream: TStringStream);
var
  Index: integer;
begin
  if frmGoPhast.PhastModel.Title.Count > 0 then
  begin
    Stream.WriteString('TITLE' + EndOfLine);
    with frmGoPhast.PhastModel.Title do
    begin
      for Index := 0 to Count - 1 do
      begin
        Stream.WriteString('.' + BlankSpaces + Strings[Index] + EndOfLine);
      end;
    end;
  end;
end;

procedure WriteSolutionMethod(const Stream: TStringStream);
begin
  Stream.WriteString('SOLUTION_METHOD' + EndOfLine);
  with frmGoPhast.PhastModel.SolutionOptions do
  begin
    if SolverType = psDirect then
    begin
      Stream.WriteString(BlankSpaces + '-direct_solver true' + EndOfLine);
    end
    else
    begin
      Stream.WriteString(BlankSpaces + '-iterative_solver true' + EndOfLine);
      Stream.WriteString(BlankSpaces + '-tolerance '
        + FloatToStr(Tolerance) + EndOfLine);
      Stream.WriteString(BlankSpaces + '-save_directions '
        + IntToStr(SaveDirections) + EndOfLine);
      Stream.WriteString(BlankSpaces + '-maximum_iterations '
        + IntToStr(MaximumIterations) + EndOfLine);
    end;
    Stream.WriteString(BlankSpaces + '-space_differencing '
      + FloatToStr(SpaceDifferencing) + EndOfLine);
    Stream.WriteString(BlankSpaces + '-time_differencing '
      + FloatToStr(TimeDifferencing) + EndOfLine);
    if CrossDispersion then
    begin
      Stream.WriteString(BlankSpaces + '-cross_dispersion true' + EndOfLine);
    end
    else
    begin
      Stream.WriteString(BlankSpaces + '-cross_dispersion false' + EndOfLine);
    end;
    Stream.WriteString(BlankSpaces + '-rebalance_fraction '
      + FloatToStr(RebalanceFraction.Value) + EndOfLine);
    if RebalanceByCell then
    begin
      Stream.WriteString(BlankSpaces + '-rebalance_by_cell true' + EndOfLine);
    end
    else
    begin
      Stream.WriteString(BlankSpaces + '-rebalance_by_cell false' + EndOfLine);
    end;
  end;
end;

procedure WritePrintInitial(const Stream: TStringStream);
  procedure WriteBooleanItem(const Item: string; const Value: boolean);
  begin
    Stream.WriteString(Item + ' ');
    if Value then
    begin
      Stream.WriteString('true' + EndOfLine);
    end
    else
    begin
      Stream.WriteString('false' + EndOfLine);
    end;
  end;
begin
  Stream.WriteString('PRINT_INITIAL' + EndOfLine);
  with frmGoPhast.PhastModel.PrintInitial do
  begin
    WriteBooleanItem(BlankSpaces + '-boundary_conditions',
      PrintInitialBoundaryConditions);
    WriteBooleanItem(BlankSpaces + '-components', PrintInitialComponents);
    WriteBooleanItem(BlankSpaces + '-conductance', PrintInitialConductance);
    WriteBooleanItem(BlankSpaces + '-echo_input', PrintInitialEchoInput);
    WriteBooleanItem(BlankSpaces + '-fluid_properties',
      PrintInitialFluidProperties);
    if frmGoPhast.PhastModel.SoluteTransport then
    begin
      WriteBooleanItem(BlankSpaces + '-force_chemistry_print',
        PrintInitialForceChemistryPrint);
      WriteBooleanItem(BlankSpaces + '-HDF_chemistry',
        PrintInitialHDF_Chemistry);
    end;
    WriteBooleanItem(BlankSpaces + '-HDF_heads', PrintInitialHDF_Heads);
    if frmGoPhast.PhastModel.SteadyFlowOptions.SteadyFlow then
    begin
      WriteBooleanItem(BlankSpaces + '-HDF_steady_flow_velocities',
        PrintInitialHDF_SteadyFlowVelocites);
    end;
    WriteBooleanItem(BlankSpaces + '-heads', PrintInitialHeads);
    WriteBooleanItem(BlankSpaces + '-media_properties',
      PrintInitialMediaProperties);
    WriteBooleanItem(BlankSpaces + '-solution_method',
      PrintInitialSolutionMethod);
    if frmGoPhast.PhastModel.SteadyFlowOptions.SteadyFlow then
    begin
      WriteBooleanItem(BlankSpaces + '-steady_flow_velocities',
        PrintInitialSteadyFlowVelocities);
    end;
    WriteBooleanItem(BlankSpaces + '-wells', PrintInitialWells);
    if frmGoPhast.PhastModel.SoluteTransport then
    begin
      WriteBooleanItem(BlankSpaces + '-xyz_chemistry',
        PrintInitialXYZ_Chemistry);
    end;
    WriteBooleanItem(BlankSpaces + '-xyz_components',
      PrintInitialXYZ_Components);
    WriteBooleanItem(BlankSpaces + '-xyz_heads', PrintInitialXYZ_Heads);
    if frmGoPhast.PhastModel.SteadyFlowOptions.SteadyFlow then
    begin
      WriteBooleanItem(BlankSpaces + '-xyz_steady_flow_velocities',
        PrintInitialXYZ_SteadyFlowVelocities);
    end;
    WriteBooleanItem(BlankSpaces + '-xyz_wells', PrintInitialXYZ_Wells);
  end;
end;

procedure WritePrintFrequency(const Stream: TStringStream);
var
  PrintFreq: TPrintFrequencyItem;
  TimeIndex: integer;
  procedure WriteUnits(const Units: TFrequencyUnits);
  begin
    case Units of
      fuDefault:
        begin
          // do nothing.
        end;
      fuSeconds:
        begin
          Stream.WriteString('seconds');
        end;
      fuMinutes:
        begin
          Stream.WriteString('minutes');
        end;
      fuHours:
        begin
          Stream.WriteString('hours');
        end;
      fuDays:
        begin
          Stream.WriteString('day');
        end;
      fuYears:
        begin
          Stream.WriteString('years');
        end;
      fuStep:
        begin
          Stream.WriteString('step');
        end;
      fuEnd:
        begin
          Stream.WriteString('end');
        end;
    else
      Assert(False);
    end;
  end;
  procedure WriteItem(const Item: string; const Value: double;
    const Units: TFrequencyUnits);
  begin
    case Units of
      fuDefault, fuSeconds, fuMinutes, fuHours, fuDays, fuYears:
        begin
          Stream.WriteString(Item + ' ' + FloatToStr(Value) + ' ');
        end;
      fuStep:
        begin
          Stream.WriteString(Item + ' ' + IntToStr(Round(Value)) + ' ');
        end;
      fuEnd:
        begin
          Stream.WriteString(Item + ' ');
        end;
    else
      Assert(False);
    end;
    WriteUnits(Units);
    Stream.WriteString(EndOfLine);
  end;
begin
  if frmGoPhast.PhastModel.PrintFrequency.Count = 0 then
  begin
    Exit;
  end;
  Stream.WriteString('PRINT_FREQUENCY' + EndOfLine);
  for TimeIndex := 0 to frmGoPhast.PhastModel.PrintFrequency.Count - 1 do
  begin
    PrintFreq := frmGoPhast.PhastModel.PrintFrequency.Items[TimeIndex] as
      TPrintFrequencyItem;
    with PrintFreq do
    begin
      WriteTime(Stream, Time);
      Stream.WriteString(EndOfLine);
      WriteItem(BlankSpaces + '-bc_flow_rates', BC_FlowRates,
        BC_FlowRatesUnits);
      Stream.WriteString(BlankSpaces + '-boundary_conditions ');
      if BoundaryConditions then
      begin
        Stream.WriteString('true' + EndOfLine);
      end
      else
      begin
        Stream.WriteString('false' + EndOfLine);
      end;
      WriteItem(BlankSpaces + '-components', Components, ComponentsUnits);
      WriteItem(BlankSpaces + '-conductance', Conductances, ConductancesUnits);
      WriteItem(BlankSpaces + '-flow_balance', FlowBalance, FlowBalanceUnits);
      if frmGoPhast.PhastModel.SoluteTransport then
      begin
        WriteItem(BlankSpaces + '-force_chemistry_print', ForceChemistryPrint,
          ForceChemistryPrintUnits);
        WriteItem(BlankSpaces + '-HDF_chemistry', HDF_Chemistry,
          HDF_ChemistryUnits);
      end;
      WriteItem(BlankSpaces + '-HDF_heads', HDF_Heads, HDF_HeadsUnits);
      WriteItem(BlankSpaces + '-HDF_velocities', HDF_Velocities,
        HDF_VelocitiesUnits);
      WriteItem(BlankSpaces + '-heads', Heads, HeadsUnits);
      WriteItem(BlankSpaces + '-progress_statistics', ProgressStatistics,
        ProgressStatisticsUnits);
      WriteItem(BlankSpaces + '-restart', RestartFrequency.Value,
        RestartFrequencyUnits);
      if Time = frmGoPhast.PhastModel.ModelTimes
        [frmGoPhast.PhastModel.ModelTimes.Count - 2] then
      begin
        Stream.WriteString(BlankSpaces + '-save_final_heads ');
        if frmGoPhast.PhastModel.PrintFrequency.SaveFinalHeads then
        begin
          Stream.WriteString('true' + EndOfLine);
        end
        else
        begin
          Stream.WriteString('false' + EndOfLine);
        end;
      end;
      WriteItem(BlankSpaces + '-velocities', Velocities, VelocitiesUnits);
      WriteItem(BlankSpaces + '-wells', Wells, WellsUnits);
      if frmGoPhast.PhastModel.SoluteTransport then
      begin
        WriteItem(BlankSpaces + '-xyz_chemistry', XYZ_Chemistry,
          XYZ_ChemistryUnits);
      end;
      WriteItem(BlankSpaces + '-xyz_components', XYZ_Components,
        XYZ_ComponentsUnits);
      WriteItem(BlankSpaces + '-xyz_heads', XYZ_Heads, XYZ_HeadsUnits);
      WriteItem(BlankSpaces + '-xyz_velocities', XYZ_Velocities,
        XYZ_VelocitiesUnits);
      WriteItem(BlankSpaces + '-xyz_wells', XYZ_Wells, XYZ_WellsUnits);
      Stream.WriteString(BlankSpaces + '-end_of_period_default ');
      if EndOfPeriodDefault then
      begin
        Stream.WriteString('true' + EndOfLine);
      end
      else
      begin
        Stream.WriteString('false' + EndOfLine);
      end;
    end;
  end;
end;

procedure WriteFreeSurface(const Stream: TStringStream);
begin
  Stream.WriteString('FREE_SURFACE_BC ');
  if frmGoPhast.PhastModel.FreeSurface then
  begin
    Stream.WriteString('true' + EndOfLine);
  end
  else
  begin
    Stream.WriteString('false' + EndOfLine);
  end;
end;

{procedure WriteFluidProperties(const Stream: TStringStream);
begin
  Stream.WriteString('FLUID_PROPERTIES' + EndOfLine);
  with frmGoPhast.Model.FluidProperties do
  begin
    Stream.WriteString(BlankSpaces + '-compressibility '
      + FloatToStr(FluidCompressibility) + ' # 1/Pa' + EndOfLine);
    Stream.WriteString(BlankSpaces + '-density '
      + FloatToStr(FluidDensity) + ' # kg/m^3' + EndOfLine);
    Stream.WriteString(BlankSpaces + '-diffusivity '
      + FloatToStr(FluidDiffusivity) + ' # m^2/s' + EndOfLine);
    Stream.WriteString(BlankSpaces + '-viscosity '
      + FloatToStr(FluidViscosity) + ' # Pa-sec' + EndOfLine);
  end;
end;  }

procedure WriteSoluteTransport(const Stream: TStringStream);
begin
  Stream.WriteString('SOLUTE_TRANSPORT ');
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    Stream.WriteString('true' + EndOfLine);
    Stream.WriteString(BlankSpaces + '-diffusivity' + ' '
      + FloatToStr(frmGoPhast.PhastModel.Diffusivity) + EndOfLine);
  end
  else
  begin
    Stream.WriteString('false' + EndOfLine);
  end;
end;

procedure WriteRProperty(const Stream: TStringStream;
  const PropertyName: string; const RProperty: TRProperty);
begin
  if PropertyName <> '' then
  begin
    Stream.WriteString(BlankSpaces + BlankSpaces + '-' + PropertyName + ' ');
  end;
  if RProperty.Interpolation.UsePHAST_Interpolation then
  begin
    case RProperty.Interpolation.InterpolationDirection of
      pidX:
        begin
          Stream.WriteString('X ');
        end;
      pidY:
        begin
          Stream.WriteString('Y ');
        end;
      pidZ:
        begin
          Stream.WriteString('Z ');
        end;
    else
      Assert(False);
    end;
    Stream.WriteString(
      FloatToStr(RProperty.Interpolation.RealValue1) + ' ' +
      FloatToStr(RProperty.Interpolation.Distance1) + ' ' +
      FloatToStr(RProperty.Interpolation.RealValue2) + ' ' +
      FloatToStr(RProperty.Interpolation.Distance2) + EndOfLine);
  end
  else
  begin
    Stream.WriteString(FloatToStr(RProperty.RValue) + EndOfLine);
  end;
end;

procedure WriteFraction(const Stream: TStringStream; const Zone: TCustomPhastZone;
  const DataSet: TDataArray);
var
  XStart, XEnd, YStart, YEnd, ZStart, ZEnd: integer;
  Frac: double;
  EvaluatedAt: TEvaluatedAt;
  XIndex, YIndex, ZIndex: integer;
  IntegerPhastDataSet: TIntegerPhastDataSet;
  SparseIntegerPhastDataSet: TSparseIntegerPhastDataSet;
  Counter: integer;
  First: boolean;
  PreviousNumber: double;
  NumberCounter: integer;
  FirstLine: boolean;
  Temp: boolean;
  procedure WritePrevious;
  begin
    if Counter mod 10 = 0 then
    begin
      if FirstLine then
      begin
        FirstLine := False;
      end
      else
      begin
        Stream.WriteString(BlankSpaces + BlankSpaces + BlankSpaces);
      end;
    end;
    if NumberCounter <> 1 then
    begin
      Stream.WriteString(IntToStr(NumberCounter) + '*');
    end;
    Stream.WriteString(
      FloatToStr(PreviousNumber) + ' ');
    Inc(Counter);
    if Counter mod 10 = 0 then
    begin
      Stream.WriteString(EndOfLine);
    end;
  end;
  procedure WriteNum;
  begin
    if (Frac <> PreviousNumber) then
    begin
      if not First then
      begin
        WritePrevious;
      end;
      PreviousNumber := Frac;
      NumberCounter := 1;
    end
    else
    begin
      Inc(NumberCounter);
    end;
    First := False;
  end;
begin
  EvaluatedAt := DataSet.EvaluatedAt;
  Stream.WriteString(BlankSpaces + BlankSpaces + BlankSpaces + '< ');
  FirstLine := True;
  case EvaluatedAt of
    eaBlocks:
      begin
        Assert(False);
      end;
    eaNodes:
      begin
        XStart := frmGoPhast.PhastGrid.NearestColumnPosition(Zone.FX1);
        if XStart < 0 then
        begin
          XStart := 0;
        end;
        if XStart > frmGoPhast.PhastGrid.ColumnCount then
        begin
          XStart := frmGoPhast.PhastGrid.ColumnCount;
        end;
        if Zone.FX1 > frmGoPhast.PhastGrid.ColumnPosition[XStart] then
        begin
          Inc(XStart);
        end;
        XEnd := frmGoPhast.PhastGrid.NearestColumnPosition(Zone.FX2);
        if XEnd < 0 then
        begin
          XEnd := 0;
        end;
        if XEnd > frmGoPhast.PhastGrid.ColumnCount then
        begin
          XEnd := frmGoPhast.PhastGrid.ColumnCount;
        end;
        if Zone.FX2 < frmGoPhast.PhastGrid.ColumnPosition[XEnd] then
        begin
          Dec(XEnd);
        end;
        YStart := frmGoPhast.PhastGrid.NearestRowPosition(Zone.FY1);
        if YStart < 0 then
        begin
          YStart := 0;
        end;
        if YStart > frmGoPhast.PhastGrid.RowCount then
        begin
          YStart := frmGoPhast.PhastGrid.RowCount;
        end;
        if Zone.FY1 > frmGoPhast.PhastGrid.RowPosition[YStart] then
        begin
          Inc(YStart);
        end;
        YEnd := frmGoPhast.PhastGrid.NearestRowPosition(Zone.FY2);
        if YEnd < 0 then
        begin
          YEnd := 0;
        end;
        if YEnd > frmGoPhast.PhastGrid.RowCount then
        begin
          YEnd := frmGoPhast.PhastGrid.RowCount;
        end;
        if Zone.FY2 < frmGoPhast.PhastGrid.RowPosition[YEnd] then
        begin
          Dec(YEnd);
        end;
        ZStart := frmGoPhast.PhastGrid.NearestLayerPosition(Zone.FZ1);
        if ZStart < 0 then
        begin
          ZStart := 0;
        end;
        if ZStart > frmGoPhast.PhastGrid.LayerCount then
        begin
          ZStart := frmGoPhast.PhastGrid.LayerCount;
        end;
        if Zone.FZ1 > frmGoPhast.PhastGrid.LayerElevation[ZStart] then
        begin
          Inc(ZStart);
        end;
        ZEnd := frmGoPhast.PhastGrid.NearestLayerPosition(Zone.FZ2);
        if ZEnd < 0 then
        begin
          ZEnd := 0;
        end;
        if ZEnd > frmGoPhast.PhastGrid.LayerCount then
        begin
          ZEnd := frmGoPhast.PhastGrid.LayerCount;
        end;
        if Zone.FZ2 < frmGoPhast.PhastGrid.LayerElevation[ZEnd] then
        begin
          Dec(ZEnd);
        end;
        Counter := 0;
        First := True;
        NumberCounter := 0;
        PreviousNumber := 0;
        if DataSet is TIntegerPhastDataSet then
        begin
          IntegerPhastDataSet := TIntegerPhastDataSet(DataSet);
          for ZIndex := ZStart to ZEnd do
          begin
            for YIndex := YStart to YEnd do
            begin
              for XIndex := XStart to XEnd do
              begin
                Frac := IntegerPhastDataSet.Fraction[ZIndex, YIndex, XIndex];
                WriteNum;
              end;
            end;
          end;
        end
        else if DataSet is TSparseIntegerPhastDataSet then
        begin
          SparseIntegerPhastDataSet := TSparseIntegerPhastDataSet(DataSet);
          for ZIndex := ZStart to ZEnd do
          begin
            for YIndex := YStart to YEnd do
            begin
              for XIndex := XStart to XEnd do
              begin
                Frac := SparseIntegerPhastDataSet.Fraction[ZIndex, YIndex,
                  XIndex];
                WriteNum;
              end;
            end;
          end;
        end
        else
        begin
          Assert(false);
        end;
        Temp := FirstLine;
        WritePrevious;
        if (Counter mod 10 = 0) and not Temp then
        begin
          Stream.WriteString(BlankSpaces + BlankSpaces + BlankSpaces);
        end;
        Stream.WriteString('>' + EndOfLine);
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure WriteIProperty(const Stream: TStringStream;
  const PropertyName: string; const Zone: TCustomPhastZone;
  const IProperty: TIProperty; const DataSet: TDataArray);
begin
  if PropertyName <> '' then
  begin
    Stream.WriteString(BlankSpaces + BlankSpaces + '-' + PropertyName + ' ');
  end;
  if IProperty.Interpolation.UsePHAST_Interpolation then
  begin
    case IProperty.Interpolation.InterpolationDirection of
      pidX:
        begin
          Stream.WriteString('X ');
        end;
      pidY:
        begin
          Stream.WriteString('Y ');
        end;
      pidZ:
        begin
          Stream.WriteString('Z ');
        end;
      pidMix:
        begin
          Stream.WriteString('mixture ');
        end;
    else
      Assert(False);
    end;
    if IProperty.Interpolation.InterpolationDirection = pidMix then
    begin
      Stream.WriteString(
        FloatToStr(IProperty.Interpolation.IntValue1) + ' ' +
        FloatToStr(IProperty.Interpolation.IntValue2) + EndOfLine);
      WriteFraction(Stream, Zone, DataSet);
    end
    else
    begin
      Stream.WriteString(
        FloatToStr(IProperty.Interpolation.IntValue1) + ' ' +
        FloatToStr(IProperty.Interpolation.Distance1) + ' ' +
        FloatToStr(IProperty.Interpolation.IntValue2) + ' ' +
        FloatToStr(IProperty.Interpolation.Distance2) + EndOfLine);
    end;
  end
  else
  begin
    Stream.WriteString(FloatToStr(IProperty.IValue) + EndOfLine);
  end;
end;

procedure WriteBProperty(const Stream: TStringStream;
  const PropertyName: string; const RProperty: TBProperty);
begin
  Stream.WriteString(BlankSpaces + BlankSpaces + '-' + PropertyName + ' ');
  if RProperty.BValue then
  begin
    Stream.WriteString('1');
  end
  else
  begin
    Stream.WriteString('0');
  end;
  Stream.WriteString(EndOfLine);
end;

procedure WriteMedia(const Stream: TStringStream);
var
  MediaZones: TMediaZoneGroup;
  ActiveZones: TActiveZoneGroup;
  ZoneIndex: integer;
  MediaZone: TMediaZone;
  ActiveZone: TActiveZone;
begin
  MediaZones := TMediaZoneGroup.Create;
  ActiveZones := TActiveZoneGroup.Create;
  try
    Stream.WriteString('MEDIA' + EndOfLine);
    for ZoneIndex := 0 to MediaZones.ZoneCount - 1 do
    begin
      MediaZone := MediaZones.Zones[ZoneIndex];
      MediaZone.WriteZone(Stream);
      WriteRProperty(Stream, 'Kx', MediaZone.Kx);
      WriteRProperty(Stream, 'Ky', MediaZone.Ky);
      WriteRProperty(Stream, 'Kz', MediaZone.Kz);
      WriteRProperty(Stream, 'porosity', MediaZone.Porosity);
      WriteRProperty(Stream, 'specific_storage', MediaZone.SpecificStorage);
      if frmGoPhast.PhastModel.SoluteTransport then
      begin
        WriteRProperty(Stream, 'longitudinal_dispersivity',
          MediaZone.LongDisp);
        WriteRProperty(Stream, 'horizontal_dispersivity',
          MediaZone.HorzTransDisp);
        WriteRProperty(Stream, 'vertical_dispersivity',
          MediaZone.VertTransDisp);
      end;

    end;
    for ZoneIndex := 0 to ActiveZones.ZoneCount - 1 do
    begin
      ActiveZone := ActiveZones.Zones[ZoneIndex];
      ActiveZone.WriteZone(Stream);
      WriteBProperty(Stream, 'active', ActiveZone.FActive);
    end;
  finally
    MediaZones.Free;
    ActiveZones.Free;
  end;
end;

procedure WriteChemistryIC(const Stream: TStringStream);
var
  InitialChemistryZones: TInitialChemistryZoneGroup;
  InitialChemistryZone: TInitialChemistryZone;
  ZoneIndex: integer;
  DataSet: TDataArray;
  DataArrayManager: TDataArrayManager;
begin
  InitialChemistryZones := TInitialChemistryZoneGroup.Create;
  try
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    Stream.WriteString('CHEMISTRY_IC' + EndOfLine);
    for ZoneIndex := 0 to InitialChemistryZones.ZoneCount - 1 do
    begin
      InitialChemistryZone := InitialChemistryZones.Zones[ZoneIndex];
      InitialChemistryZone.WriteZone(Stream);
      DataSet := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Solution);
      WriteIProperty(Stream, 'solution', InitialChemistryZone,
        InitialChemistryZone.FChemistry_Initial_Solution, DataSet);
      if frmGoPhast.PhastModel.ChemistryOptions.UseEquilibriumPhases then
      begin
        DataSet := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Equilibrium_Phases);
        WriteIProperty(Stream, 'equilibrium_phases', InitialChemistryZone,
          InitialChemistryZone.FChemistry_Initial_Equilibrium_Phases, DataSet);
      end;
      if frmGoPhast.PhastModel.ChemistryOptions.UseSurfaceAssemblages then
      begin
        DataSet := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Surface);
        WriteIProperty(Stream, 'surface', InitialChemistryZone,
          InitialChemistryZone.FChemistry_Initial_Surface, DataSet);
      end;
      if frmGoPhast.PhastModel.ChemistryOptions.UseExchange then
      begin
        DataSet := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Exchange);
        WriteIProperty(Stream, 'exchange', InitialChemistryZone,
          InitialChemistryZone.FChemistry_Initial_Exchange, DataSet);
      end;
      if frmGoPhast.PhastModel.ChemistryOptions.UseGasPhases then
      begin
        DataSet := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Gas_Phase);
        WriteIProperty(Stream, 'gas_phase', InitialChemistryZone,
          InitialChemistryZone.FChemistry_Initial_Gas_Phase, DataSet);
      end;
      if frmGoPhast.PhastModel.ChemistryOptions.UseSolidSolution then
      begin
        DataSet := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Solid_Solutions);
        WriteIProperty(Stream, 'solid_solutions', InitialChemistryZone,
          InitialChemistryZone.FChemistry_Initial_Solid_Solutions, DataSet);
      end;
      if frmGoPhast.PhastModel.ChemistryOptions.UseKineticReactants then
      begin
        DataSet := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Kinetics);
        WriteIProperty(Stream, 'kinetics', InitialChemistryZone,
          InitialChemistryZone.FChemistry_Initial_Kinetics, DataSet);
      end;
    end;
  finally
    InitialChemistryZones.Free;
  end;
end;

procedure WriteHeadIC(const Stream: TStringStream);
var
  InitialHeadZones: TInitialHeadZoneGroup;
  InitialHeadZone: TInitialHeadZone;
  ZoneIndex: integer;
  ElementCount: integer;
  Zone: TCustomPhastZone;
  LayerIndex, RowIndex, ColIndex: integer;
  DataSet: TDataArray;
//  DataSetIndex: integer;
  PointIndex: integer;
begin
  Stream.WriteString('HEAD_IC' + EndOfLine);
  if frmGoPhast.PhastModel.DoInitialWaterTableUsed(nil) then
  begin
    DataSet := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(rsInitial_Water_Table);
//    DataSet := frmGoPhast.PhastModel.DataSets[DataSetIndex];
    DataSet.Initialize;
    with frmGoPhast.PhastModel.PhastGrid do
    begin

      // PHAST may no longer supports the water_table option.
      Zone := TCustomPhastZone.Create;
      try
        Zone.FX1 := ColumnPosition[0];
        Zone.FX2 := ColumnPosition[ColumnCount];
        Zone.FY1 := RowPosition[0];
        Zone.FY2 := RowPosition[RowCount];
        Zone.FZ1 := LayerElevation[0];
        Zone.FZ2 := LayerElevation[LayerCount];
        Zone.WriteZone(Stream);
      finally
        Zone.Free;
      end;

      Stream.WriteString(BlankSpaces + BlankSpaces + '-head by_node' +
        EndOfLine);
      Stream.WriteString(BlankSpaces + BlankSpaces + '<' + EndOfLine);

      PointIndex := 0;
      for LayerIndex := 0 to LayerCount do
      begin
        for RowIndex := 0 to RowCount do
        begin
          for ColIndex := 0 to ColumnCount do
          begin
            if PointIndex mod 10 = 0 then
            begin
              Stream.WriteString(BlankSpaces + BlankSpaces + BlankSpaces);
            end;
            Inc(PointIndex);
            Stream.WriteString(FloatToStr(DataSet.RealData[0,
              RowIndex, ColIndex]) + ' ');
            if PointIndex mod 10 = 0 then
            begin
              Stream.WriteString(EndOfLine);
            end;
          end;
        end;
      end;
      if PointIndex mod 10 <> 0 then
      begin
        Stream.WriteString(EndOfLine);
      end;
      Stream.WriteString(BlankSpaces + BlankSpaces + '>' + EndOfLine);

    // This is how the water table used to be printed.

//      Stream.WriteString(BlankSpaces + BlankSpaces + '-water_table by_node' +
//        EndOfLine);
//      Stream.WriteString(BlankSpaces + BlankSpaces + '<' + EndOfLine);
//      PointIndex := 0;
//      for RowIndex := 0 to RowCount do
//      begin
//        for ColIndex := 0 to ColumnCount do
//        begin
//          if PointIndex mod 10 = 0 then
//          begin
//            Stream.WriteString(BlankSpaces + BlankSpaces + BlankSpaces);
//          end;
//          Inc(PointIndex);
//          Stream.WriteString(FloatToStr(DataSet.RealData[0,
//            RowIndex, ColIndex]) + ' ');
//          if PointIndex mod 10 = 0 then
//          begin
//            Stream.WriteString(EndOfLine);
//          end;
//        end;
//      end;
//      if PointIndex mod 10 <> 0 then
//      begin
//        Stream.WriteString(EndOfLine);
//      end;
//      Stream.WriteString(BlankSpaces + BlankSpaces + '>' + EndOfLine);
    end;
  end
  else
  begin
    InitialHeadZones := TInitialHeadZoneGroup.Create;
    try
      with frmGoPhast.PhastModel.PhastGrid do
      begin
        ElementCount := ColumnCount * RowCount * LayerCount;
      end;
      if InitialHeadZones.ZoneCount > ElementCount div 10 then
      begin
        DataSet := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(rsInitial_Head);
//        DataSet := frmGoPhast.PhastModel.DataSets[DataSetIndex];
        with frmGoPhast.PhastModel.PhastGrid do
        begin
          Zone := TCustomPhastZone.Create;
          try
            Zone.FX1 := ColumnPosition[0];
            Zone.FX2 := ColumnPosition[ColumnCount];
            Zone.FY1 := RowPosition[0];
            Zone.FY2 := RowPosition[RowCount];
            Zone.FZ1 := LayerElevation[0];
            Zone.FZ2 := LayerElevation[LayerCount];
            Zone.WriteZone(Stream);
          finally
            Zone.Free;
          end;
          Stream.WriteString(BlankSpaces + BlankSpaces + '-head by_node' +
            EndOfLine);
          Stream.WriteString(BlankSpaces + BlankSpaces + '<' + EndOfLine);

          PointIndex := 0;
          for LayerIndex := 0 to LayerCount do
          begin
            for RowIndex := 0 to RowCount do
            begin
              for ColIndex := 0 to ColumnCount do
              begin
                if PointIndex mod 10 = 0 then
                begin
                  Stream.WriteString(BlankSpaces + BlankSpaces + BlankSpaces);
                end;
                Inc(PointIndex);
                Stream.WriteString(FloatToStr(DataSet.RealData[LayerIndex,
                  RowIndex, ColIndex]) + ' ');
                if PointIndex mod 10 = 0 then
                begin
                  Stream.WriteString(EndOfLine);
                end;
              end;
            end;
          end;
          if PointIndex mod 10 <> 0 then
          begin
            Stream.WriteString(EndOfLine);
          end;
        end;
        Stream.WriteString(BlankSpaces + BlankSpaces + '>' + EndOfLine);
      end
      else
      begin
        for ZoneIndex := 0 to InitialHeadZones.ZoneCount - 1 do
        begin
          InitialHeadZone := InitialHeadZones.Zones[ZoneIndex];
          InitialHeadZone.WriteZone(Stream);
          WriteRProperty(Stream, 'head', InitialHeadZone.FInitialHead);
        end;
      end;
    finally
      InitialHeadZones.Free;
    end;
  end;
end;

procedure WritePrintLocations(const Stream: TStringStream;
  const PrintChemistryZones: TPrintChemistryZoneGroup;
  const PrintChemistryXYZ_Zones: TPrintChemistryXYZ_ZoneGroup);
var
  PrintChemistryZone: TPrintChemistryZone;
  PrintChemistryXYZ_Zone: TPrintChemistryXYZ_Zone;
  ZoneIndex: integer;
  DataSet: TDataArray;
  DataArrayManager: TDataArrayManager;
begin
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    Stream.WriteString('PRINT_LOCATIONS' + EndOfLine);
    Stream.WriteString(BlankSpaces + '-chemistry' + EndOfLine);
    for ZoneIndex := 0 to PrintChemistryZones.ZoneCount - 1 do
    begin
      PrintChemistryZone := PrintChemistryZones.Zones[ZoneIndex];
      PrintChemistryZone.WriteZone(Stream);
      DataSet := DataArrayManager.GetDataSetByName(rsPrint_Chemistry);
      if PrintChemistryZone.FPrint_Chemistry.IValue > 1 then
      begin
        WriteIProperty(Stream, 'sample X', PrintChemistryZone,
          PrintChemistryZone.FPrint_Chemistry, DataSet);
        WriteIProperty(Stream, 'sample Y', PrintChemistryZone,
          PrintChemistryZone.FPrint_Chemistry, DataSet);
        WriteIProperty(Stream, 'sample Z', PrintChemistryZone,
          PrintChemistryZone.FPrint_Chemistry, DataSet);
      end
      else
      begin
        WriteIProperty(Stream, 'print', PrintChemistryZone,
          PrintChemistryZone.FPrint_Chemistry, DataSet);
      end;
    end;
    Stream.WriteString(BlankSpaces + '-xyz_chemistry' + EndOfLine);
    for ZoneIndex := 0 to PrintChemistryXYZ_Zones.ZoneCount - 1 do
    begin
      PrintChemistryXYZ_Zone := PrintChemistryXYZ_Zones.Zones[ZoneIndex];
      PrintChemistryXYZ_Zone.WriteZone(Stream);
      DataSet := DataArrayManager.GetDataSetByName(rsPrint_XYZ_Chemistry);
      if PrintChemistryXYZ_Zone.FPrint_ChemistryXYZ.IValue > 1 then
      begin
        WriteIProperty(Stream, 'sample X', PrintChemistryXYZ_Zone,
          PrintChemistryXYZ_Zone.FPrint_ChemistryXYZ, DataSet);
        WriteIProperty(Stream, 'sample Y', PrintChemistryXYZ_Zone,
          PrintChemistryXYZ_Zone.FPrint_ChemistryXYZ, DataSet);
        WriteIProperty(Stream, 'sample Z', PrintChemistryXYZ_Zone,
          PrintChemistryXYZ_Zone.FPrint_ChemistryXYZ, DataSet);
      end
      else
      begin
        WriteIProperty(Stream, 'print', PrintChemistryXYZ_Zone,
          PrintChemistryXYZ_Zone.FPrint_ChemistryXYZ, DataSet);
      end;
    end;
  end;
end;

procedure WritePhastGrid(const Stream: TStringStream);
  procedure WriteDirection(const Direction: string;
    const Positions: TOneDRealArray);
    function Width(const I: integer): double;
    begin
      result := Positions[I + 1] - Positions[I];
    end;
  var
    Index: integer;
    AWidth: double;
    Uniform: boolean;
    ALine: string;
  begin
    Assert(length(Positions) > 0);
    AWidth := Width(0);
    Uniform := True;
    for Index := 1 to Length(Positions) - 2 do
    begin
      if AWidth <> Width(Index) then
      begin
        Uniform := False;
        break;
      end;
    end;
    if Uniform then
    begin
      ALine := BlankSpaces + '-uniform   ' + Direction + '   ' +
        FloatToStr(Positions[0]) + ' ' +
        FloatToStr(Positions[Length(Positions) - 1]) + ' ' +
      IntToStr(Length(Positions));
    end
    else
    begin
      ALine := BlankSpaces + '-nonuniform   ' + Direction + '   ';
      for Index := 0 to Length(Positions) - 1 do
      begin
        ALine := ALine + ' ' + FloatToStr(Positions[Index]);
        if (Index > 0) and ((Index mod 10) = 0) then
        begin
          Stream.WriteString(ALine + EndOfLine);
          ALine := BlankSpaces + BlankSpaces;
        end;
      end;
    end;
    if Trim(ALine) <> '' then
    begin
      Stream.WriteString(ALine + EndOfLine);
    end;
  end;
begin
  Stream.WriteString('GRID' + EndOfLine);
  WriteDirection('X', frmGoPhast.PhastGrid.ColumnPositions);
  WriteDirection('Y', frmGoPhast.PhastGrid.RowPositions);
  WriteDirection('Z', frmGoPhast.PhastGrid.LayerElevations);
  // write chemistry dimensions
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    Stream.WriteString(BlankSpaces + '-chemistry_dimensions ');
    with frmGoPhast.PhastModel.GridOptions do
    begin
      if ChemicalDimensionX then
      begin
        Stream.WriteString('X');
      end;
      if ChemicalDimensionY then
      begin
        Stream.WriteString('Y');
      end;
      if ChemicalDimensionZ then
      begin
        Stream.WriteString('Z');
      end;
    end;
    Stream.WriteString(EndOfLine);
  end;
  Stream.WriteString(BlankSpaces + '-print_orientation ');
  case frmGoPhast.PhastModel.GridOptions.PrintOrientation of
    pgXY:
      begin
        Stream.WriteString('XY');
      end;
    pgXZ:
      begin
        Stream.WriteString('XZ');
      end;
  else
    Assert(False);
  end;
  Stream.WriteString(EndOfLine);
end;

procedure WriteSpecifiedValues(const Stream: TStringStream;
  const SpecifiedHeadZones: TSpecifiedHeadZoneGroup);
var
  ZoneIndex: integer;
  SpecifiedHeadZone: TSpecifiedHeadZone;
  RProperty: TRProperty;
  IProperty: TIProperty;
  DataSet: TDataArray;
  TimeIndex: integer;
  SolutionString: string;
  Time: double;
  HeadIndex, AssociatedSolutionIndex: integer;
begin
  if (SpecifiedHeadZones.ZoneCount = 0) then
  begin
    Exit;
  end;
  Stream.WriteString('SPECIFIED_HEAD_BC' + EndOfLine);
  for ZoneIndex := 0 to SpecifiedHeadZones.ZoneCount - 1 do
  begin
    SpecifiedHeadZone := SpecifiedHeadZones.Zones[ZoneIndex];
    SpecifiedHeadZone.WriteZone(Stream);
    WritePropertyName(Stream, 'head');
    for HeadIndex := 0 to SpecifiedHeadZones.HeadTimeCount - 1 do
    begin
      Time := SpecifiedHeadZones.HeadTimes[HeadIndex];
      RProperty := SpecifiedHeadZone.FSpecifiedHeads[HeadIndex];
      WriteTime(Stream, Time);
      WriteRProperty(Stream, '', RProperty);
    end;
    if frmGoPhast.PhastModel.SoluteTransport then
    begin
      if SpecifiedHeadZone.FSolutionType.IValue = 0 then
      begin
        SolutionString := 'associated_solution';
      end
      else
      begin
        SolutionString := 'fixed_solution';
      end;
      WritePropertyName(Stream, SolutionString);
      for AssociatedSolutionIndex := 0 to SpecifiedHeadZones.SolutionTimeCount -
        1 do
      begin
        Time := SpecifiedHeadZones.SolutionTimes[AssociatedSolutionIndex];
        IProperty :=
          SpecifiedHeadZone.FAssociatedSolutions[AssociatedSolutionIndex];
        TimeIndex :=
          frmGoPhast.PhastModel.SpecifiedHeadAssociatedSolution.IndexOf(Time);
        DataSet :=
          frmGoPhast.PhastModel.SpecifiedHeadAssociatedSolution.Items[TimeIndex];
        WriteTime(Stream, Time);
        WriteIProperty(Stream, '', SpecifiedHeadZone, IProperty, DataSet);
      end;
    end;
  end;
end;

procedure WriteSpecifiedFluxes(const Stream: TStringStream;
  const TopFluxZones: TSpecifiedFluxTopGroup;
  const FrontFluxZones: TSpecifiedFluxFrontGroup;
  const SideFluxZones: TSpecifiedFluxSideGroup);
var
  ZoneIndex: integer;
  SpecifiedFluxZone: TBoundaryZone;
  RProperty: TRProperty;
  IProperty: TIProperty;
  TimeIndex: integer;
  DataSet: TDataArray;
  Time: double;
  TopFluxIndex, FrontFluxIndex, SideFluxIndex: integer;
  TopFluxSolutionIndex, FrontFluxSolutionIndex, SideFluxSolutionIndex: integer;
begin
  if (TopFluxZones.ZoneCount = 0)
    and (FrontFluxZones.ZoneCount = 0)
    and (SideFluxZones.ZoneCount = 0) then
  begin
    Exit;
  end;
  Stream.WriteString('FLUX_BC' + EndOfLine);
  if TopFluxZones.ZoneCount > 0 then
  begin
    for ZoneIndex := 0 to TopFluxZones.ZoneCount - 1 do
    begin
      SpecifiedFluxZone := TopFluxZones.Zones[ZoneIndex];
      SpecifiedFluxZone.WriteZone(Stream);
      Stream.WriteString(BlankSpaces + BlankSpaces + '-face Z' + EndOfLine);
      WritePropertyName(Stream, 'flux');
      for TopFluxIndex := 0 to TopFluxZones.BoundaryTimeCount - 1 do
      begin
        Time := TopFluxZones.BoundaryTimes[TopFluxIndex];
        RProperty := SpecifiedFluxZone.FBoundaries[TopFluxIndex];
        WriteTime(Stream, Time);
        WriteRProperty(Stream, '', RProperty);
      end;
      if frmGoPhast.PhastModel.SoluteTransport then
      begin
        WritePropertyName(Stream, 'associated_solution');
        for TopFluxSolutionIndex := 0 to TopFluxZones.SolutionTimeCount - 1 do
        begin
          Time := TopFluxZones.SolutionTimes[TopFluxSolutionIndex];
          TimeIndex := frmGoPhast.PhastModel.TopFluxBoundaryChemistry.IndexOf(Time);
          DataSet := frmGoPhast.PhastModel.TopFluxBoundaryChemistry.Items[TimeIndex];
          IProperty :=
            SpecifiedFluxZone.FAssociatedSolutions[TopFluxSolutionIndex];
          WriteTime(Stream, Time);
          WriteIProperty(Stream, '', SpecifiedFluxZone, IProperty, DataSet);
        end;
      end;
    end;
  end;
  if FrontFluxZones.ZoneCount > 0 then
  begin
    for ZoneIndex := 0 to FrontFluxZones.ZoneCount - 1 do
    begin
      SpecifiedFluxZone := FrontFluxZones.Zones[ZoneIndex];
      SpecifiedFluxZone.WriteZone(Stream);
      Stream.WriteString(BlankSpaces + BlankSpaces + '-face Y' + EndOfLine);
      WritePropertyName(Stream, 'flux');
      for FrontFluxIndex := 0 to FrontFluxZones.BoundaryTimeCount - 1 do
      begin
        Time := FrontFluxZones.BoundaryTimes[FrontFluxIndex];
        RProperty := SpecifiedFluxZone.FBoundaries[FrontFluxIndex];
        WriteTime(Stream, Time);
        WriteRProperty(Stream, '', RProperty);
      end;
      if frmGoPhast.PhastModel.SoluteTransport then
      begin
        WritePropertyName(Stream, 'associated_solution');
        for FrontFluxSolutionIndex := 0 to FrontFluxZones.SolutionTimeCount - 1
          do
        begin
          Time := FrontFluxZones.SolutionTimes[FrontFluxSolutionIndex];
          TimeIndex :=
            frmGoPhast.PhastModel.FrontFluxBoundaryChemistry.IndexOf(Time);
          DataSet :=
            frmGoPhast.PhastModel.FrontFluxBoundaryChemistry.Items[TimeIndex];
          IProperty :=
            SpecifiedFluxZone.FAssociatedSolutions[FrontFluxSolutionIndex];
          WriteTime(Stream, Time);
          WriteIProperty(Stream, '', SpecifiedFluxZone, IProperty, DataSet);
        end;
      end;
    end;
  end;
  if SideFluxZones.ZoneCount > 0 then
  begin
    for ZoneIndex := 0 to SideFluxZones.ZoneCount - 1 do
    begin
      SpecifiedFluxZone := SideFluxZones.Zones[ZoneIndex];
      SpecifiedFluxZone.WriteZone(Stream);
      Stream.WriteString(BlankSpaces + BlankSpaces + '-face X' + EndOfLine);
      WritePropertyName(Stream, 'flux');
      for SideFluxIndex := 0 to SideFluxZones.BoundaryTimeCount - 1 do
      begin
        Time := SideFluxZones.BoundaryTimes[SideFluxIndex];
        RProperty := SpecifiedFluxZone.FBoundaries[SideFluxIndex];
        WriteTime(Stream, Time);
        WriteRProperty(Stream, '', RProperty);
      end;
      if frmGoPhast.PhastModel.SoluteTransport then
      begin
        WritePropertyName(Stream, 'associated_solution');
        for SideFluxSolutionIndex := 0 to SideFluxZones.SolutionTimeCount - 1 do
        begin
          Time := SideFluxZones.SolutionTimes[SideFluxSolutionIndex];
          TimeIndex := frmGoPhast.PhastModel.SideFluxBoundaryChemistry.IndexOf(Time);
          DataSet :=
            frmGoPhast.PhastModel.SideFluxBoundaryChemistry.Items[TimeIndex];
          IProperty :=
            SpecifiedFluxZone.FAssociatedSolutions[SideFluxSolutionIndex];
          WriteTime(Stream, Time);
          WriteIProperty(Stream, '', SpecifiedFluxZone, IProperty, DataSet);
        end;
      end;
    end;
  end;
end;

procedure WriteLeakyBoundary(const Stream: TStringStream;
  const TopLeakyZones: TTopLeakyZoneGroup;
  const FrontLeakyZones: TFrontLeakyZoneGroup;
  const SideLeakyZones: TSideLeakyZoneGroup);
var
  ZoneIndex: integer;
  LeakyZone: TCustomLeakyZone;
  RProperty: TRProperty;
  IProperty: TIProperty;
  TimeIndex: integer;
  DataSet: TDataArray;
  Time: double;
  TopLeakyIndex, FrontLeakyIndex, SideLeakyIndex: integer;
  TopLeakySolutionIndex, FrontLeakySolutionIndex, SideLeakySolutionIndex:
  integer;
begin
  if (TopLeakyZones.ZoneCount = 0)
    and (FrontLeakyZones.ZoneCount = 0)
    and (SideLeakyZones.ZoneCount = 0) then
  begin
    Exit;
  end;

  Stream.WriteString('LEAKY_BC' + EndOfLine);
  for ZoneIndex := 0 to TopLeakyZones.ZoneCount - 1 do
  begin
    LeakyZone := TopLeakyZones.Zones[ZoneIndex];
    LeakyZone.WriteZone(Stream);
    Stream.WriteString(BlankSpaces + BlankSpaces + '-face Z' + EndOfLine);
    WritePropertyName(Stream, 'head');
    for TopLeakyIndex := 0 to TopLeakyZones.BoundaryTimeCount - 1 do
    begin
      Time := TopLeakyZones.BoundaryTimes[TopLeakyIndex];
      RProperty := LeakyZone.FBoundaries[TopLeakyIndex];
      WriteTime(Stream, Time);
      WriteRProperty(Stream, '', RProperty);
    end;
    if frmGoPhast.PhastModel.SoluteTransport then
    begin
      WritePropertyName(Stream, 'associated_solution');
      for TopLeakySolutionIndex := 0 to TopLeakyZones.SolutionTimeCount - 1 do
      begin
        Time := TopLeakyZones.SolutionTimes[TopLeakySolutionIndex];
        TimeIndex := frmGoPhast.PhastModel.TopLeakyAssociatedSolution.IndexOf(Time);
        DataSet := frmGoPhast.PhastModel.TopLeakyAssociatedSolution.Items[TimeIndex];
        IProperty := LeakyZone.FAssociatedSolutions[TopLeakySolutionIndex];
        WriteTime(Stream, Time);
        WriteIProperty(Stream, '', LeakyZone, IProperty, DataSet);
      end;
    end;
    RProperty := LeakyZone.FHydraulicConductivity;
    WriteRProperty(Stream, 'hydraulic_conductivity', RProperty);
    RProperty := LeakyZone.FThickness;
    WriteRProperty(Stream, 'thickness', RProperty);
  end;
  for ZoneIndex := 0 to FrontLeakyZones.ZoneCount - 1 do
  begin
    LeakyZone := FrontLeakyZones.Zones[ZoneIndex];
    LeakyZone.WriteZone(Stream);
    Stream.WriteString(BlankSpaces + BlankSpaces + '-face Y' + EndOfLine);
    WritePropertyName(Stream, 'head');
    for FrontLeakyIndex := 0 to FrontLeakyZones.BoundaryTimeCount - 1 do
    begin
      Time := FrontLeakyZones.BoundaryTimes[FrontLeakyIndex];
      RProperty := LeakyZone.FBoundaries[FrontLeakyIndex];
      WriteTime(Stream, Time);
      WriteRProperty(Stream, '', RProperty);
    end;
    if frmGoPhast.PhastModel.SoluteTransport then
    begin
      WritePropertyName(Stream, 'associated_solution');
      for FrontLeakySolutionIndex := 0 to FrontLeakyZones.SolutionTimeCount - 1
        do
      begin
        Time := FrontLeakyZones.SolutionTimes[FrontLeakySolutionIndex];
        TimeIndex :=
          frmGoPhast.PhastModel.FrontLeakyAssociatedSolution.IndexOf(Time);
        DataSet :=
          frmGoPhast.PhastModel.FrontLeakyAssociatedSolution.Items[TimeIndex];
        IProperty := LeakyZone.FAssociatedSolutions[FrontLeakySolutionIndex];
        WriteTime(Stream, Time);
        WriteIProperty(Stream, '', LeakyZone, IProperty, DataSet);
      end;
    end;
    RProperty := LeakyZone.FHydraulicConductivity;
    WriteRProperty(Stream, 'hydraulic_conductivity', RProperty);
    RProperty := LeakyZone.FThickness;
    WriteRProperty(Stream, 'thickness', RProperty);
  end;
  for ZoneIndex := 0 to SideLeakyZones.ZoneCount - 1 do
  begin
    LeakyZone := SideLeakyZones.Zones[ZoneIndex];
    LeakyZone.WriteZone(Stream);
    Stream.WriteString(BlankSpaces + BlankSpaces + '-face X' + EndOfLine);
    WritePropertyName(Stream, 'head');
    for SideLeakyIndex := 0 to SideLeakyZones.BoundaryTimeCount - 1 do
    begin
      Time := SideLeakyZones.BoundaryTimes[SideLeakyIndex];
      RProperty := LeakyZone.FBoundaries[SideLeakyIndex];
      WriteTime(Stream, Time);
      WriteRProperty(Stream, '', RProperty);
    end;
    if frmGoPhast.PhastModel.SoluteTransport then
    begin
      WritePropertyName(Stream, 'associated_solution');
      for SideLeakySolutionIndex := 0 to SideLeakyZones.SolutionTimeCount - 1 do
      begin
        Time := SideLeakyZones.SolutionTimes[SideLeakySolutionIndex];
        TimeIndex := frmGoPhast.PhastModel.SideLeakyAssociatedSolution.IndexOf(Time);
        DataSet :=
          frmGoPhast.PhastModel.SideLeakyAssociatedSolution.Items[TimeIndex];
        IProperty := LeakyZone.FAssociatedSolutions[SideLeakySolutionIndex];
        WriteTime(Stream, Time);
        WriteIProperty(Stream, '', LeakyZone, IProperty, DataSet);
      end;
    end;
    RProperty := LeakyZone.FHydraulicConductivity;
    WriteRProperty(Stream, 'hydraulic_conductivity', RProperty);
    RProperty := LeakyZone.FThickness;
    WriteRProperty(Stream, 'thickness', RProperty);
  end;
end;

procedure WriteRiver(const Stream: TStringStream;
  const RiverWriter: TRiverWriter);
var
  Index: integer;
  AScreenObject: TScreenObject;
  RiverIndex: integer;
  SegIndex: integer;
  SegmentIndex: integer;
  CellSegment: TCellElementSegment;
  PriorX, PriorY: double;
  SetDefaults: boolean;
  Width: double;
  Depth: double;
  BedConductivity: double;
  BedThickness: double;
  Solution: Integer;
  Head: double;
  Time: double;
  RiverHeadIndex, RiverSolutionIndex: integer;
  LayerToUse: integer;
  procedure WriteRiverName;
  var
    SegmentName: string;
  begin
    Inc(RiverIndex);
    if SegIndex > 1 then
    begin
      SegmentName := ' Segment ' + IntToStr(SegIndex);
    end
    else
    begin
      SegmentName := '';
    end;
    Stream.WriteString('RIVER ' + IntToStr(RiverIndex) + ' '
      + AScreenObject.RiverBoundary.Description + SegmentName + EndOfLine);
  end;
begin

  if RiverWriter.ScreenObjectList.Count > 0 then
  begin
    { TODO : 
If the layers are to be displayed at the top of the model, this needs to use 
LayerToUse := frmGoPhast.Model.PhastGrid.LayerCount. }

//    LayerToUse := frmGoPhast.Model.PhastGrid.LayerCount;
    LayerToUse := 0;
    RiverIndex := 0;
    PriorX := 0;
    PriorY := 0;
    for Index := 0 to RiverWriter.ScreenObjectList.Count - 1 do
    begin
      AScreenObject := RiverWriter.ScreenObjectList[Index];
      SegIndex := 0;
      SetDefaults := True;
      for SegmentIndex := 0 to AScreenObject.Segments[
        frmGoPhast.PhastModel].Count - 1 do
      begin
        CellSegment := AScreenObject.Segments[
          frmGoPhast.PhastModel][SegmentIndex];
        if CellSegment.Layer <> LayerToUse then
        begin
          Continue;
        end;
        SetDefaults := SetDefaults or (CellSegment.X1 <> PriorX) or
          (CellSegment.Y1 <> PriorY);
        Width := RiverWriter.Width.RealData[CellSegment.Layer,
          CellSegment.Row, CellSegment.Col];
        Depth := RiverWriter.Depth.RealData[CellSegment.Layer,
          CellSegment.Row, CellSegment.Col];
        BedConductivity :=
          RiverWriter.BedHydraulicConductivity.RealData[CellSegment.Layer,
          CellSegment.Row, CellSegment.Col];
        BedThickness := RiverWriter.BedThickness.RealData[CellSegment.Layer,
          CellSegment.Row, CellSegment.Col];
        if SetDefaults then
        begin
          Inc(SegIndex);
          WriteRiverName;
          Stream.WriteString(BlankSpaces + 'point '
            + FloatToStr(CellSegment.X1) + ' '
            + FloatToStr(CellSegment.Y1)
            + ' # x and y location of point on river' + EndOfLine);
          WritePropertyName(Stream, 'head');
          for RiverHeadIndex := 0 to RiverWriter.Head.Count - 1 do
          begin
            Time := RiverWriter.Head.Times[RiverHeadIndex];
            Head := RiverWriter.Head.Items[RiverHeadIndex].
              RealData[CellSegment.Layer, CellSegment.Row, CellSegment.Col];
            Stream.WriteString(BlankSpaces + BlankSpaces + FloatToStr(Time)
              + BlankSpaces + FloatToStr(Head) + EndOfLine);
          end;
          if frmGoPhast.PhastModel.SoluteTransport then
          begin
            WritePropertyName(Stream, 'solution');
            for RiverSolutionIndex := 0 to RiverWriter.AssociatedSolution.Count
              - 1 do
            begin
              Time := RiverWriter.AssociatedSolution.Times[RiverSolutionIndex];
              Solution :=
                RiverWriter.AssociatedSolution.Items[RiverSolutionIndex].
                IntegerData[CellSegment.Layer, CellSegment.Row,
                CellSegment.Col];
              Stream.WriteString(BlankSpaces + BlankSpaces + FloatToStr(Time)
                + BlankSpaces + IntToStr(Solution) + EndOfLine);
            end;
          end;
          Stream.WriteString(BlankSpaces + BlankSpaces + '-width '
            + FloatToStr(Width) + EndOfLine);
          Stream.WriteString(BlankSpaces + BlankSpaces + '-depth '
            + FloatToStr(Depth) + EndOfLine);
          Stream.WriteString(BlankSpaces + BlankSpaces
            + '-bed_hydraulic_conductivity '
            + FloatToStr(BedConductivity) + EndOfLine);
          Stream.WriteString(BlankSpaces + BlankSpaces + '-bed_thickness '
            + FloatToStr(BedThickness) + EndOfLine);
        end;
        PriorX := CellSegment.X2;
        PriorY := CellSegment.Y2;
        if not SetDefaults then
        begin
          if CellSegment.StartPosition <> epFirst then
          begin
            if CellSegment.EndPosition = epLast then
            begin
              Stream.WriteString(BlankSpaces + 'point '
                + FloatToStr(CellSegment.X2) + ' '
                + FloatToStr(CellSegment.Y2)
                + ' # x and y location of point on river' + EndOfLine);
            end
            else
            begin
              Stream.WriteString(BlankSpaces + 'point '
                + FloatToStr((CellSegment.X1 + CellSegment.X2) / 2) + ' '
                + FloatToStr((CellSegment.Y1 + CellSegment.Y2) / 2)
                + ' # x and y location of point on river' + EndOfLine);
            end;
            WritePropertyName(Stream, 'head');
            for RiverHeadIndex := 0 to RiverWriter.Head.Count - 1 do
            begin
              Time := RiverWriter.Head.Times[RiverHeadIndex];
              Head := RiverWriter.Head.Items[RiverHeadIndex].
                RealData[CellSegment.Layer, CellSegment.Row, CellSegment.Col];
              Stream.WriteString(BlankSpaces + BlankSpaces + FloatToStr(Time)
                + BlankSpaces + FloatToStr(Head) + EndOfLine);
            end;
            if frmGoPhast.PhastModel.SoluteTransport then
            begin
              WritePropertyName(Stream, 'solution');
              for RiverSolutionIndex := 0 to
                RiverWriter.AssociatedSolution.Count - 1 do
              begin
                Time :=
                  RiverWriter.AssociatedSolution.Times[RiverSolutionIndex];
                Solution :=
                  RiverWriter.AssociatedSolution.Items[RiverSolutionIndex].
                  IntegerData[CellSegment.Layer, CellSegment.Row,
                  CellSegment.Col];
                Stream.WriteString(BlankSpaces + BlankSpaces + FloatToStr(Time)
                  + BlankSpaces + IntToStr(Solution) + EndOfLine);
              end;
            end;
            Stream.WriteString(BlankSpaces + BlankSpaces + '-width '
              + FloatToStr(Width) + EndOfLine);
            Stream.WriteString(BlankSpaces + BlankSpaces + '-depth '
              + FloatToStr(Depth) + EndOfLine);
            Stream.WriteString(BlankSpaces + BlankSpaces
              + '-bed_hydraulic_conductivity '
              + FloatToStr(BedConductivity) + EndOfLine);
            Stream.WriteString(BlankSpaces + BlankSpaces + '-bed_thickness '
              + FloatToStr(BedThickness) + EndOfLine);
          end;
        end;
        SetDefaults := False;
      end;
    end;
  end;
end;

procedure WriteWells(const Stream: TStringStream;
  const WellWriter: TWellWriter);
var
  Index: integer;
  AScreenObject: TScreenObject;
  SegmentIndex: integer;
  CellSegment: TCellElementSegment;
  PumpingRate: double;
  Solution: integer;
  IntervalIndex: integer;
  WellInterval: TWellInterval;
  IntervalString: string;
  Injection: boolean;
  Time: double;
  WellPumpingRateIndex, WellSolutionIndex: integer;
begin
  if WellWriter.ScreenObjectList.Count = 0 then
  begin
    Exit;
  end;
  for Index := 0 to WellWriter.ScreenObjectList.Count - 1 do
  begin
    AScreenObject := WellWriter.ScreenObjectList[Index];
    for SegmentIndex := 0 to AScreenObject.Segments[
      frmGoPhast.PhastModel].Count - 1 do
    begin
      CellSegment := AScreenObject.Segments[
        frmGoPhast.PhastModel][SegmentIndex];
      if CellSegment.Layer <> 0 then
      begin
        Continue;
      end;
      Stream.WriteString('WELL ' + IntToStr(Index + 1) + ' '
        + AScreenObject.WellBoundary.Description + EndOfLine);
      Stream.WriteString(BlankSpaces + FloatToStr(CellSegment.X2) + ' '
        + FloatToStr(CellSegment.Y2) + EndOfLine);

      Stream.WriteString(BlankSpaces + BlankSpaces + '-diameter '
        + FloatToStr(AScreenObject.WellBoundary.Diameter) + EndOfLine);

      if AScreenObject.WellBoundary.WellElevationFormat = wefDepth then
      begin
        Stream.WriteString(BlankSpaces + BlankSpaces + '-land_surface_datum '
          + FloatToStr(AScreenObject.WellBoundary.LandSurfaceDatum) +
            EndOfLine);
        IntervalString := '-depth ';
      end
      else
      begin
        IntervalString := '-elevation ';
      end;
      for IntervalIndex := 0 to AScreenObject.WellBoundary.Intervals.Count - 1
        do
      begin
        WellInterval := AScreenObject.WellBoundary.Intervals.Items[IntervalIndex]
          as TWellInterval;
        Stream.WriteString(BlankSpaces + BlankSpaces + IntervalString
          + FloatToStr(WellInterval.FirstElevation) + ' '
          + FloatToStr(WellInterval.SecondElevation) + EndOfLine);
        if AScreenObject.WellBoundary.AllocateByPressureAndMobility then
        begin
          Stream.WriteString(BlankSpaces
            + '-allocate_by_head_and_mobility true'
            + EndOfLine);
        end;
      end;

      Injection := true;
      for WellPumpingRateIndex := 0 to WellWriter.PumpingRate.Count - 1 do
      begin
        Time := WellWriter.PumpingRate.Times[WellPumpingRateIndex];
        PumpingRate := WellWriter.PumpingRate.Items[WellPumpingRateIndex].
          RealData[CellSegment.Layer, CellSegment.Row, CellSegment.Col];

        if WellPumpingRateIndex = 0 then
        begin
          if PumpingRate < 0 then
          begin
            Injection := true;
            WritePropertyName(Stream, 'injection_rate');
            // injection
          end
          else
          begin
            Injection := False;
            WritePropertyName(Stream, 'pumping_rate');
            // pumping
          end;
        end;

        WriteTime(Stream, Time);
        if Injection then
        begin
          // injection
          Stream.WriteString(BlankSpaces
            + FloatToStr(-PumpingRate) + EndOfLine);
        end
        else
        begin
          // pumping
          Stream.WriteString(BlankSpaces
            + FloatToStr(PumpingRate) + EndOfLine);
        end;
      end;

      if frmGoPhast.PhastModel.SoluteTransport then
      begin
        WritePropertyName(Stream, 'solution');
        for WellSolutionIndex := 0 to WellWriter.Solution.Count - 1 do
        begin
          Time := WellWriter.Solution.Times[WellSolutionIndex];
          Solution := WellWriter.Solution.Items[WellSolutionIndex].
            IntegerData[CellSegment.Layer, CellSegment.Row, CellSegment.Col];
          WriteTime(Stream, Time);
          Stream.WriteString(BlankSpaces
            + IntToStr(Solution) + EndOfLine);
        end;
      end;
      break;
    end;
  end;
end;

procedure WritePhastInput(const FileName: string; RunModel: boolean);
var
  OldDecSep: Char;
  Input: TStringStream;
  InputFile: TFileStream;
  SpecifiedHeadZones: TSpecifiedHeadZoneGroup;
  PrintChemistryZones: TPrintChemistryZoneGroup;
  PrintChemistryXYZ_Zones: TPrintChemistryXYZ_ZoneGroup;
  TopFluxZones: TSpecifiedFluxTopGroup;
  FrontFluxZones: TSpecifiedFluxFrontGroup;
  SideFluxZones: TSpecifiedFluxSideGroup;
  TopLeakyZones: TTopLeakyZoneGroup;
  FrontLeakyZones: TFrontLeakyZoneGroup;
  SideLeakyZones: TSideLeakyZoneGroup;
  RiverWriter: TRiverWriter;
  WellWriter: TWellWriter;
  TimeUnits: string;
  PriorUpToDate: boolean;
  FileRoot: string;
  BatchFile: TStringList;
  FileDir: string;
  BatchName: string;
  PhastLocation: string;
  ADir: string;
  GeoRefWriter: TGeoRefWriter;
  ADirectory: string;
  AFileName: string;
  RootName: string;
  Extensions: TStringList;
  ExtensionIndex: Integer;
  AnExt: string;
  ArchiveFileName: string;
  PhastDir: string;
  PhastFiles: TStringDynArray;
  FileIndex: Integer;
  ModelName: string;
  PrintFrequency: TPrintFrequencyCollection;
  PrintInitial: TPrintInitial;
  NetworkDrive: Boolean;
  ModelDirectory: string;
begin
  try
    PhastLocation := frmGoPhast.PhastModel.ProgramLocations.PhastLocation;
    if not FileExists(PhastLocation) then
    begin
      frmGoPhast.miPHASTProgramLocationClick(nil);
      PhastLocation := frmGoPhast.PhastModel.ProgramLocations.PhastLocation;
      if not FileExists(PhastLocation) then
      begin
        Exit;
      end;
    end;
    frmGoPhast.PhastModel.ClearModelFiles;
    PhastDir := ExtractFileDir(PhastLocation);
    PhastFiles := TDirectory.GetFiles(PhastDir);
    for FileIndex := 0 to Length(PhastFiles) -1 do
    begin
      frmGoPhast.PhastModel.AddBinaryFile(PhastFiles[FileIndex]);
    end;

    ADir := ExtractFileDir(FileName);
    SetCurrentDir(ADir);

    PriorUpToDate := frmGoPhast.PhastModel.UpToDate;
    OldDecSep := FormatSettings.DecimalSeparator;
    try
      try
        frmGoPhast.PhastModel.AddModelInputFile(FileName);
        ADirectory := ExtractFileDir(FileName);
        ADirectory := IncludeTrailingPathDelimiter(ADirectory);
        if frmGoPhast.PhastModel.SoluteTransport then
        begin
          frmGoPhast.PhastModel.AddModelInputFile(ADirectory + 'phast.dat');
        end;
        AFileName  := ExtractFileName(FileName);
        AFileName := ChangeFileExt(AFileName , '');
        AFileName := ChangeFileExt(AFileName , '');
        RootName := AFileName;
        if frmGoPhast.PhastModel.SoluteTransport then
        begin
          AFileName := AFileName + '.chem.dat';
          frmGoPhast.PhastModel.AddModelInputFile(ADirectory + AFileName);
        end;

        FormatSettings.DecimalSeparator := '.';
        Input := TStringStream.Create('');
        SpecifiedHeadZones := nil;
        TopFluxZones := nil;
        FrontFluxZones := nil;
        SideFluxZones := nil;
        TopLeakyZones := nil;
        FrontLeakyZones := nil;
        SideLeakyZones := nil;
        PrintChemistryZones := nil;
        PrintChemistryXYZ_Zones := nil;
        RiverWriter := nil;
        WellWriter := nil;
        try
          WriteTitle(Input);
          WritePrintInitial(Input);
          WriteSoluteTransport(Input);
          WriteUnits(Input, TimeUnits);
          WritePhastGrid(Input);
          WriteMedia(Input);
          WriteFreeSurface(Input);
          WriteSteadyFlow(Input);
          WriteSolutionMethod(Input);
          WriteHeadIC(Input);
          if frmGoPhast.PhastModel.SoluteTransport then
          begin
            WriteChemistryIC(Input);
          end;
          frmGoPhast.PhastModel.InitializeTimes;

          SpecifiedHeadZones := TSpecifiedHeadZoneGroup.Create;
          TopFluxZones := TSpecifiedFluxTopGroup.Create;
          FrontFluxZones := TSpecifiedFluxFrontGroup.Create;
          SideFluxZones := TSpecifiedFluxSideGroup.Create;
          TopLeakyZones := TTopLeakyZoneGroup.Create;
          FrontLeakyZones := TFrontLeakyZoneGroup.Create;
          SideLeakyZones := TSideLeakyZoneGroup.Create;
          RiverWriter := TRiverWriter.Create;
          WellWriter := TWellWriter.Create;
          if frmGoPhast.PhastModel.SoluteTransport then
          begin
            PrintChemistryZones := TPrintChemistryZoneGroup.Create;
            PrintChemistryXYZ_Zones := TPrintChemistryXYZ_ZoneGroup.Create;
          end;
          try
            WritePrintFrequency(Input);
            if frmGoPhast.PhastModel.SoluteTransport then
            begin
              WritePrintLocations(Input, PrintChemistryZones,
                PrintChemistryXYZ_Zones);
            end;

            frmErrorsAndWarnings.BeginUpdate;
            try
              frmErrorsAndWarnings.RemoveErrorGroup(frmGoPhast.PhastModel,
                StrNoBoundaryConditio);
              if (TopLeakyZones.ZoneCount = 0)
                and (FrontLeakyZones.ZoneCount = 0)
                and (SideLeakyZones.ZoneCount = 0)
                and (TopFluxZones.ZoneCount = 0)
                and (FrontFluxZones.ZoneCount = 0)
                and (SideFluxZones.ZoneCount = 0)
                and (SpecifiedHeadZones.ZoneCount = 0)
                and (RiverWriter.ScreenObjectList.Count = 0)
                and (WellWriter.ScreenObjectList.Count = 0)
                then
              begin
                frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
                  StrNoBoundaryConditio, StrYouMustSpecifyAt);
              end;

              GeoRefWriter := TGeoRefWriter.Create(frmGoPhast.PhastModel, etExport);
              try
                GeoRefWriter.WriteFile(FileName, smtMain);
              finally
                GeoRefWriter.Free;
              end;

            finally
              frmErrorsAndWarnings.EndUpdate;
            end;

            WriteLeakyBoundary(Input, TopLeakyZones, FrontLeakyZones,
              SideLeakyZones);
            WriteSpecifiedFluxes(Input, TopFluxZones, FrontFluxZones,
              SideFluxZones);
            WriteSpecifiedValues(Input, SpecifiedHeadZones);
            WriteRiver(Input, RiverWriter);
            WriteWells(Input, WellWriter);

            WriteTimeControl(Input, TimeUnits);
            WriteEnd(Input);
          finally
            PrintChemistryZones.Free;
            PrintChemistryXYZ_Zones.Free;
          end;
          InputFile := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite,
            ReadWritePermissions);
          try
            Input.Position := 0;
            InputFile.CopyFrom(Input, Input.Size)
          finally
            InputFile.Free;
          end;
        finally
          Input.Free;
          SpecifiedHeadZones.Free;
          TopFluxZones.Free;
          FrontFluxZones.Free;
          SideFluxZones.Free;
          TopLeakyZones.Free;
          FrontLeakyZones.Free;
          SideLeakyZones.Free;
          RiverWriter.Free;
          WellWriter.Free;
        end;
      except on E: EFCreateError do
        begin
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      FormatSettings.DecimalSeparator := OldDecSep;
      frmGoPhast.PhastModel.RestoreColoredDataSets;
      frmGoPhast.PhastModel.UpToDate := PriorUpToDate;
    end;
    FileDir := IncludeTrailingPathDelimiter(ExtractFileDir(FileName));
    BatchName :=  FileDir + 'RunPhast.bat';
    NetworkDrive := IsNetworkDrive(FileName);
    ModelDirectory := ExtractFileDir(FileName);

    FileRoot := ExtractFileName(FileName);
    // strip off first extension.
    FileRoot := ChangeFileExt(FileRoot, '');
    // strip off second extension.
    FileRoot := ChangeFileExt(FileRoot, '');
    BatchFile := TStringList.Create;
    try
      if NetworkDrive then
      begin
        BatchFile.Add('pushd ' + ModelDirectory);
      end;
      BatchFile.AddStrings(frmGoPhast.PhastModel.BatchFileAdditionsBeforeModel);

      if Pos(' ', PhastLocation) > 0 then
      begin
        PhastLocation := '"' + PhastLocation + '"';
      end;
      ADir := IncludeTrailingPathDelimiter(ADir);

  //    BatchFile.Add('call ' + PhastLocation + ' ' + ADir + FileRoot);
      BatchFile.Add('call ' + PhastLocation + ' ' + FileRoot);
      BatchFile.AddStrings(frmGoPhast.PhastModel.BatchFileAdditionsAfterModel);
      if NetworkDrive then
      begin
        BatchFile.Add('popd');
      end;
      BatchFile.Add('pause');
      try
        BatchFile.SaveToFile(BatchName);
      except on E: EFCreateError do
        begin
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;

      BatchFile.Clear;

      BatchFile.Add('if not exist "..\..\output\NUL" mkdir "..\..\output"');
      ModelName := ExtractFileName(ChangeFileExt(FileName, ''));
      ModelName := ExtractFileName(ChangeFileExt(ModelName, ''));
      BatchFile.Add(Format('if not exist "..\..\output\%0:s\NUL" mkdir "..\..\output\%0:s"', [ModelName]));

      BatchFile.AddStrings(frmGoPhast.PhastModel.BatchFileAdditionsBeforeModel);
      BatchFile.Add('call ..\..\bin\phast.bat ' + FileRoot);
      BatchFile.AddStrings(frmGoPhast.PhastModel.BatchFileAdditionsAfterModel);

      Extensions := TStringList.Create;
      try
        PrintFrequency := frmGoPhast.PhastModel.PrintFrequency;
        PrintInitial := frmGoPhast.PhastModel.PrintInitial;

        if PrintFrequency.SaveFinalHeads then
        begin
          Extensions.Add('.head.dat');
        end;
        if PrintInitial.PrintInitialEchoInput
          or PrintFrequency.LogPrinted then
        begin
          Extensions.Add('.log');
        end;
        if PrintFrequency.OBalPrinted then
        begin
          Extensions.Add('.O.bal');
        end;
        if PrintFrequency.BcfPrinted then
        begin
          Extensions.Add('.O.bcf');
        end;
        if frmGoPhast.PhastModel.SoluteTransport then
        begin
          if PrintInitial.PrintInitialForceChemistryPrint
            or PrintFrequency.OChemPrinted then
          begin
            Extensions.Add('.O.chem');
          end;
          if PrintInitial.PrintInitialComponents
            or PrintFrequency.OCompPrinted then
          begin
            Extensions.Add('.O.comps');
          end;
        end;
        if  PrintInitial.PrintInitialHeads
          or PrintFrequency.OHeadPrinted then
        begin
          Extensions.Add('.O.head');
        end;
        if PrintInitial.PrintInitialConductance
            or PrintFrequency.KdPrinted then
        begin
          Extensions.Add('.O.kd');
        end;
        if PrintInitial.PrintInitialBoundaryConditions
          or PrintInitial.PrintInitialMediaProperties
          or PrintInitial.PrintInitialSolutionMethod
          or PrintInitial.PrintInitialFluidProperties
          or PrintFrequency.ProbDefPrinted
          then
        begin
          Extensions.Add('.O.probdef');
        end;
        if PrintInitial.PrintInitialSteadyFlowVelocities then
        begin
          Extensions.Add('.O.vel');
        end;
        if PrintInitial.PrintInitialWells
          or PrintFrequency.OWelPrinted then
        begin
          Extensions.Add('.O.wel');
        end;
//        Extensions.Add('.sel');
        if frmGoPhast.PhastModel.SoluteTransport then
        begin
          if PrintInitial.PrintInitialXYZ_Chemistry
            or PrintFrequency.XyzChemPrinted then
          begin
            Extensions.Add('.xyz.chem');
          end;
          if PrintInitial.PrintInitialXYZ_Components
            or PrintFrequency.XyzCompsPrinted then
          begin
            Extensions.Add('.xyz.comps');
          end;
        end;
        if PrintInitial.PrintInitialXYZ_Heads
          or PrintFrequency.XyzHeadPrinted then
        begin
          Extensions.Add('.xyz.head');
        end;
        if PrintInitial.PrintInitialXYZ_SteadyFlowVelocities
          or PrintFrequency.XyzVelPrinted then
        begin
          Extensions.Add('.xyz.vel');
        end;
        if frmGoPhast.PhastModel.SoluteTransport then
        begin
          if PrintInitial.PrintInitialXYZ_Wells
            or PrintFrequency.XyzWelPrinted then
          begin
            Extensions.Add('.xyz.wel');
          end;
        end;
        if PrintInitial.PrintInitialHDF_Chemistry
          or PrintInitial.PrintInitialHDF_Heads
          or PrintInitial.PrintInitialHDF_SteadyFlowVelocites
          or PrintFrequency.H5Printed
          then
        begin
          Extensions.Add('.h5');
        end;
        if PrintFrequency.RestartPrinted then
        begin
          Extensions.Add('.restart.backup.gz');
          Extensions.Add('.restart.gz');
        end;

        for ExtensionIndex := 0 to Extensions.Count - 1 do
        begin
          AnExt := Extensions[ExtensionIndex];

          frmGoPhast.PhastModel.AddModelOutputFile(ADirectory + RootName + AnExt);

          BatchFile.Add('if exist ' + RootName + AnExt + ' (');
          BatchFile.Add('move /Y ' + RootName + AnExt + ' ..\..\output\' + RootName + '\'+ RootName + AnExt);
          BatchFile.Add(')');
        end;
        BatchFile.Add('pause');
      finally
        Extensions.Free;
      end;

      try
        BatchFile.SaveToFile(BatchName + ArchiveExt);
        frmGoPhast.PhastModel.AddModelInputFile(BatchName + ArchiveExt);
      except on E: EFCreateError do
        begin
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      BatchFile.Free;
  //    ReclaimMemory;
    end;

    ArchiveFileName := ChangeFileExt(FileName, '');
    frmGoPhast.PhastModel.SaveArchiveList(ChangeFileExt(ArchiveFileName, '.axml'));

    if RunModel then
    begin
      SetCurrentDir(FileDir);
      try
        RunAProgram('"' + BatchName + '"');
  //      WinExec(PAnsiChar(AnsiString('"' + BatchName + '"')), SW_SHOW);
      finally
        SetCurrentDir(ADir);
      end;
    end;
  finally
    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;

end;

end.


