unit SutraInputWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, JclAnsiStrings, SutraOptionsUnit,
  SutraMeshUnit, SutraOutputControlUnit, Generics.Collections,
  Generics.Defaults, SysUtils, Classes, GoPhastTypes,
  SutraGeneralFlowNodesUnit;

type
  TNodeData = class(TObject)
    Number: Integer;
    NREG: Integer;
    X: Double;
    Y: double;
    Z: double;
    Porosity: double;
    COMPMA: double;
    CS: double;
    RHOS: double;
    PRODL0: double;
    PRODS0: double;
    PRODL1: double;
    PRODS1: double;
    PRODI: double;
    Layer: Integer;
  end;

  TNodeDataList = TObjectList<TNodeData>;

  TNodeDataComparer = TComparer<TNodeData>;

  TElementData = class(TObject)
    Number: Integer;
    LREG: Integer;
    PMAX: double;
    PMID: double;
    PMIN: double;
    ANGLE1: double;
    ANGLE2: double;
    ANGLE3: double;
    ALMAX: double;
    ALMID: double;
    ALMIN: double;
    ATMAX: double;
    ATMID: double;
    ATMIN: double;
    SIGMAS: double;
    SIGMAA: double;
    // X, Y, and Z are used when importing model results.
    X: Double;
    Y: double;
    Z: Double;
    Layer: Integer;
  end;

  TElementDataList = TObjectList<TElementData>;

  TElementDataComparer = TComparer<TElementData>;

  TSutraInputWriter = class(TCustomFileWriter)
  private
    FOptions: TSutraOptions;
    FNOBS: Integer;
    FMesh: TSutraMesh3D;
    FOutputControl: TSutraOutputControl;
    FFluidSourceNodes: IBoundaryNodes;
    FMassEnergySourceNodes: IBoundaryNodes;
    FSpecifiedPressureNodes: IBoundaryNodes;
    FSpecifiedTempConcNodes: IBoundaryNodes;
    FFileName: string;
    FSchedules: TStringList;
    FObservations: TStringList;
    FGeneralFlowNodes: TObjectList<TList<IGeneralFlowNodes>>;
    FGeneralTransportNodes: TObjectList<TList<IGeneralTransportNodes>>;
    F_NN: Integer;
    FHasLakes: Boolean;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2A;
    procedure WriteDataSet2B;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7A;
    procedure WriteDataSet7B;
    procedure WriteDataSet7C;
    procedure WriteDataSet8A;
    procedure WriteDataSet8B;
    procedure WriteDataSet8C;
    procedure WriteDataSet8D;
    procedure WriteDataSet8E;
    procedure WriteDataSet9;
    procedure WriteDataSet10;
    procedure WriteDataSet11;
    procedure WriteDataSet11A(ARegion: TRegionalProperty);
    procedure WriteDataSet11B(ARegion: TRegionalProperty);
    procedure WriteDataSet11C(ARegion: TRegionalProperty);
    procedure WriteDataSet11D(ARegion: TRegionalProperty);
    procedure WriteDataSet11E(ARegion: TRegionalProperty);
    procedure WriteDataSet12;
    procedure WriteDataSet13;
    procedure WriteDataSet14A;
    procedure WriteDataSet14B;
    procedure WriteDataSet15A;
    procedure WriteDataSet15B;
    procedure WriteDataSet17;
    procedure WriteDataSet18;
    procedure WriteDataSet19;
    procedure WriteDataSet20;
    procedure WriteDataSet21A;
    procedure WriteDataSet21B;
    procedure WriteDataSet22;
    procedure SetHasLakes(const Value: Boolean);
    procedure WriteFileInternal;
    function EvaluateFormula(Formula: string; const DataIdentifier: string;
      out PestParamName: string): double;
  protected
    class function Extension: string; override;
  public
    property HasLakes: Boolean read FHasLakes write SetHasLakes;
    Constructor Create(AModel: TCustomModel); reintroduce;
    procedure WriteFile(FileName: string; FluidSourceNodes,
      MassEnergySourceNodes, SpecifiedPressureNodes,
      SpecifiedTempConcNodes: IBoundaryNodes; NOBS: integer;
      Schedules, Observations: TStringList;
      GeneralFlowNodes: TObjectList<TList<IGeneralFlowNodes>>;
      GeneralTransportNodes: TObjectList<TList<IGeneralTransportNodes>>);
  end;


implementation

uses
  DataSetUnit, SutraFileWriterUnit, frmErrorsAndWarningsUnit, PlProcUnit,
  ModflowParameterUnit, OrderedCollectionUnit, RbwParser,
  frmFormulaErrorsUnit, frmGoPhastUnit, DataSetNamesUnit;

resourcestring
  StrMaxPermMinPerm = 'Maximum permeability < Minimum permeability';
  StrMaxKMinK = 'Maximum hydraulic conductivity < Minimum hydraulic conducti' +
  'vity';
  StrMaxPermMidPerm = 'Maximum permeability < Middle permeability';
  StrMaxKMidK = 'Maximum hydraulic conductivity < Middle hydraulic conductiv' +
  'ity';
  StrMidPermMinPerm = 'Middle permeability < Minimum permeability';
  StrMidKMinK = 'Middle hydraulic conductivity < Minimum hydraulic conductiv' +
  'ity';
  StrDirectSolverUsed = 'Direct solver used with a large model';
  StrTheDirectSolverPressure = 'The direct solver is used for the pressure s' +
  'olution. The direct solver uses much more memory than the iterative solve' +
  'r and may cause SUTRA to run out of memory. Usually one of the iterative ' +
  'solvers is a better choice for a model with over 1000 nodes.';
  StrTheDirectSolverConc = 'The direct solver is used for the solute or heat' +
  ' solution. The direct solver uses much more memory than the iterative sol' +
  'ver and may cause SUTRA to run out of memory. Usually one of the iterative ' +
  'solvers is a better choice for a model with over 1000 nodes.';
  StrLakesUsedWithIrre = 'Lakes used with irregular mesh';
  StrSomeLayersInThis = 'Some layers in this model pinch out which forces SU' +
  'TRA to reat this model as having an irregular mesh. Lakes have been defin' +
  'ed for the model but lakes are only allowed in models with layered meshes' +
  ' not irregular meshes.';
//  StrDispersivityMayBe = 'Dispersivity may be too low at the following eleme' +      resourcestring
  StrErrorInTheFormula = 'Error in the formula for %0:s. Error message was "' +
  '%1:s". "0" will be used instead.';
  StrInvalidValuesOfPE = 'Invalid values of PENT and PSWRES';
  StrInRegion0dThe = 'In region %0:d, the value of PSWRES (%1:g) is greater ' +
  'than or equal to PENT (%2:g) in the water saturation properties.';

//  'nts. See section 7.2 of the SUTRA documentation.';

{ TSutraInputWriter }

constructor TSutraInputWriter.Create(AModel: TCustomModel);
begin
  inherited Create(AModel, etExport);
  FOptions := AModel.SutraOptions;
  FOutputControl := AModel.SutraOutputControl;
  FMesh := AModel.SutraMesh;
end;

procedure TSutraInputWriter.WriteDataSet1;
var
  TITLE1: AnsiString;
  TITLE2: AnsiString;
  StringList: TJclAnsiStringList;
  LineIndex: Integer;
  ALine: AnsiString;
begin
  StringList := TJclAnsiStringList.Create;
  try
    StringList.Text := FOptions.TitleLines;
    if StringList.Count > 0 then
    begin
      TITLE1 := StringList[0];
    end
    else
    begin
      TITLE1 := '_';
    end;
    WriteCommentLine('Data set 1');

    WriteString(TITLE1);
    NewLine;

    if StringList.Count > 1 then
    begin
      TITLE2 := StringList[1];
    end
    else
    begin
      TITLE2 := '_';
    end;
    WriteString(TITLE2);
    NewLine;

    for LineIndex := 2 to StringList.Count - 1 do
    begin
      ALine := '# ' + StringList[LineIndex];
      WriteString(ALine);
      NewLine;
    end;
  finally
    StringList.Free;
  end;
  // Data set 1
end;

procedure TSutraInputWriter.WriteDataSet10;
var
  COMPMA: Double;
  CS: Double;
  SIGMAS: Double;
  RHOS: Double;
begin
  if Model.ModelSelection = msSutra40 then
  begin
    Exit;
  end;
  WriteCommentLine('Data set 10');
  COMPMA := FOptions.MatrixCompressibility;

  CS := 0.;
  SIGMAS := 0.;
  RHOS := 0.;
  case FOptions.TransportChoice of
    tcSolute, tcSoluteHead:
      begin
        CS := 0;
        SIGMAS := 0;
        if FOptions.SorptionModel = smNone then
        begin
          RHOS := 0;
        end
        else
        begin
          RHOS := FOptions.SolidGrainDensity;
        end;
      end;
    tcEnergy, tcFreezing:
      begin
        CS := FOptions.SolidGrainSpecificHeat;
        SIGMAS := FOptions.SolidGrainDiffusivity;
        RHOS := FOptions.SolidGrainDensity;
      end;
    else
      Assert(False);
  end;
  WriteFloat(COMPMA);
  WriteFloat(CS);
  WriteFloat(SIGMAS);
  WriteFloat(RHOS);
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet11;
var
  ADSMOD: AnsiString;
  CHI1: double;
  CHI2: double;
  RegionIndex: Integer;
  NR: Integer;
  ARegion: TRegionalProperty;
begin
  // Implement as a series of Panes either in the SUTRA Options dialog box or a separate dialog box.
  // Consider giving each set of data a name that could be used as a global variable
  // to specify node and element region numbers.
  if Model.ModelSelection = msSutra40 then
  begin
    WriteCommentLine('Data set 11');
    WriteInteger(FOptions.RegionalProperties.Count);
    WriteString('  # NPMREG');
    NewLine;
    for RegionIndex := 0 to FOptions.RegionalProperties.Count - 1 do
    begin
      NR := RegionIndex + 1;
      WriteInteger(NR);
      WriteString(' # NR');
      NewLine;

      ARegion := FOptions.RegionalProperties[RegionIndex];
      WriteDataSet11A(ARegion);
      WriteDataSet11B(ARegion);
      WriteDataSet11C(ARegion);
      WriteDataSet11D(ARegion);
      WriteDataSet11E(ARegion);
    end;
  end
  else
  begin
    WriteCommentLine('Data set 11');
    case FOptions.TransportChoice of
      tcSolute, tcSoluteHead:
        begin
          ADSMOD := '';
          CHI1 := 0.;
          CHI2 := 0.;
          case FOptions.SorptionModel of
            smNone:
              begin
                ADSMOD := '''NONE'' ';
                CHI1 := 0.;
                CHI2 := 0.;
              end;
            smLinear:
              begin
                ADSMOD := '''LINEAR'' ';
                CHI1 := FOptions.FirstDistributionCoefficient;
                CHI2 := 0.;
              end;
            smFreundlich:
              begin
                ADSMOD := '''FREUNDLICH'' ';
                CHI1 := FOptions.FirstDistributionCoefficient;
                CHI2 := FOptions.SecondDistributionCoefficient;
              end;
            smLangmuir:
              begin
                ADSMOD := '''LANGMUIR'' ';
                CHI1 := FOptions.FirstDistributionCoefficient;
                CHI2 := FOptions.SecondDistributionCoefficient;
              end;
            else
              Assert(False);
          end;
          WriteString(ADSMOD);
          if FOptions.SorptionModel <> smNone then
          begin
            WriteFloat(CHI1);
            WriteFloat(CHI2);
          end;
        end;
      tcEnergy, tcFreezing:
        begin
          ADSMOD := '''NONE''';
          WriteString(ADSMOD);
        end;
      else Assert(False);
    end;
    NewLine;
  end;
end;

procedure TSutraInputWriter.WriteDataSet11A(ARegion: TRegionalProperty);
var
  AbsorbptionProperties: TAdsorptionProperties;
  Value: Double;
  PestParam: string;
begin
  AbsorbptionProperties := ARegion.AdsorptionProperties;
  if FOptions.TransportChoice in [tcSolute, tcSoluteHead] then
  begin
    case AbsorbptionProperties.AdsorptionModel of
      smNone:
        begin
          WriteString('''NONE'' ');
          WriteString(' # DATASET 11a: ADSMOD');
        end;
      smLinear:
        begin
          WriteString('''LINEAR'' ');
        end;
      smFreundlich:
        begin
          WriteString('''FREUNDLICH'' ');
        end;
      smLangmuir:
        begin
          WriteString('''LANGMUIR'' ');
        end;
      else
        Assert(False);
    end;
    if AbsorbptionProperties.AdsorptionModel in [smLinear, smFreundlich, smLangmuir] then
    begin
      Value := EvaluateFormula(AbsorbptionProperties.FirstDistributionCoefficient,
         'DATASET 11a: CHI1, FirstDistributionCoefficient', PestParam);
      WriteFormulaOrValueBasedOnAPestName(PestParam,
        Value, -1, -1, -1);

      Value := EvaluateFormula(AbsorbptionProperties.SecondDistributionCoefficient,
         'DATASET 11a: CHI2, SecondDistributionCoefficient', PestParam);
      WriteFormulaOrValueBasedOnAPestName(PestParam,
        Value, -1, -1, -1);
//      WriteFloat(AbsorbptionProperties.FirstDistributionCoefficient);
//      WriteFloat(AbsorbptionProperties.SecondDistributionCoefficient);
      WriteString(' # DATASET 11a: ADSMOD CHI1, CHI2');
    end;
  end
  else
  begin
    Assert(FOptions.TransportChoice in [tcEnergy, tcFreezing]);
    case AbsorbptionProperties.ThermalConductivityModel of
      tcmAritnmetic:
        begin
          WriteString('''ARITHM'' ');
        end;
      tcmGeometric:
        begin
          WriteString('''GEOMET'' ');
        end;
      tcmHarmonic:
        begin
          WriteString('''HARMON'' ');
        end;
      else
        Assert(False);
    end;
    WriteString(' # DATASET 11a: TCMOD');
  end;
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet11B(ARegion: TRegionalProperty);
var
  WaterSaturationProperties: TWaterSaturationProperties;
  Index: Integer;
  Value: Double;
  PestParam: string;
  PSWRES_Value: Double;
  PENT_Value: Double;
begin
  if (FOptions.TransportChoice = tcFreezing)
    or (FOptions.SaturationChoice = scUnsaturated) then
  begin
     WaterSaturationProperties := ARegion.WaterSaturationProperties;
     case WaterSaturationProperties.WaterSaturationChoice of
       wscNone:
         begin
           WriteString('''NONE'' ');
           WriteString(' # DATASET 11B: SWMOD');
         end;
       wscVanGenuchten:
         begin
           WriteString('''VGEN'' ');

           Value := EvaluateFormula(WaterSaturationProperties.ResidualWaterContent,
             'DATASET 11B: Residual total water saturation (SWRES)', PestParam);
           WriteFormulaOrValueBasedOnAPestName(PestParam,
             Value, -1, -1, -1);

           Value := EvaluateFormula(WaterSaturationProperties.VanGenuchtenAlpha,
             'DATASET 11B: van Genuchten function parameter alpha_VG (AA)', PestParam);
           WriteFormulaOrValueBasedOnAPestName(PestParam,
             Value, -1, -1, -1);

           Value := EvaluateFormula(WaterSaturationProperties.VanGenuchtenExponent,
             'DATASET 11B: van Genuchten function parameter n_VG (VN)', PestParam);
           WriteFormulaOrValueBasedOnAPestName(PestParam,
             Value, -1, -1, -1);

//           WriteFloat(WaterSaturationProperties.VanGenuchtenAlpha);
//           WriteFloat(WaterSaturationProperties.VanGenuchtenExponent);
           WriteString(' # DATASET 11B: SWMOD AA VN');
         end;
       wscBrooksCorey:
         begin
           WriteString('''BCOR'' ');

           Value := EvaluateFormula(WaterSaturationProperties.ResidualWaterContent,
             'DATASET 11B: Residual total water saturation (SWRES)', PestParam);
           WriteFormulaOrValueBasedOnAPestName(PestParam,
             Value, -1, -1, -1);

           Value := EvaluateFormula(WaterSaturationProperties.AirEntryPressure,
             'DATASET 11B: Air-entry pressure (PENT)', PestParam);
           WriteFormulaOrValueBasedOnAPestName(PestParam,
             Value, -1, -1, -1);

           Value := EvaluateFormula(WaterSaturationProperties.PoreSizeDistributionIndex,
             'DATASET 11B: Pore size distribution index (RLAMB)', PestParam);
           WriteFormulaOrValueBasedOnAPestName(PestParam,
             Value, -1, -1, -1);

//           WriteFloat(WaterSaturationProperties.ResidualWaterContent);
//           WriteFloat(WaterSaturationProperties.AirEntryPressure);
//           WriteFloat(WaterSaturationProperties.PoreSizeDistributionIndex);
           WriteString(' # DATASET 11B: SWMOD SWRES PENT RLAMB');
         end;
       wscPiecewiseLinear:
         begin
           WriteString('''PLIN'' ');
           Value := EvaluateFormula(WaterSaturationProperties.ResidualWaterContent,
             'DATASET 11B: Residual total water saturation (SWRES)', PestParam);
           WriteFormulaOrValueBasedOnAPestName(PestParam,
             Value, -1, -1, -1);

           PENT_Value := EvaluateFormula(WaterSaturationProperties.AirEntryPressure,
             'DATASET 11B: Air-entry pressure (PENT)', PestParam);
           WriteFormulaOrValueBasedOnAPestName(PestParam,
             PENT_Value, -1, -1, -1);

           PSWRES_Value := EvaluateFormula(WaterSaturationProperties.PressureForResidualWaterContent,
             'DATASET 11B: Pressure at which the saturation reaches the residual saturation (PSWRES)', PestParam);
           WriteFormulaOrValueBasedOnAPestName(PestParam,
             PSWRES_Value, -1, -1, -1);

           if PSWRES_Value >= PENT_Value then
           begin
             frmErrorsAndWarnings.AddError(Model, StrInvalidValuesOfPE,
               Format(StrInRegion0dThe,
               [ARegion.Index + 1, PSWRES_Value, PENT_Value]));
           end;

//           WriteFloat(WaterSaturationProperties.ResidualWaterContent);
//           WriteFloat(WaterSaturationProperties.AirEntryPressure);
//           WriteFloat(WaterSaturationProperties.PressureForResidualWaterContent);
           WriteString(' # DATASET 11B: SWMOD SWRES PENT PSWRES');
         end;
       wscUserDefined:
         begin
           WriteString('''UDEF'' ');
           WriteInteger(WaterSaturationProperties.FunctionParameters.Count);
           for Index := 0 to WaterSaturationProperties.FunctionParameters.Count - 1 do
           begin
             WriteFloat(WaterSaturationProperties.FunctionParameters[Index].Value);
           end;
           WriteString(' # DATASET 11B: SWMOD NSWPAR SWPAR(0..N)');
         end;
       else
         Assert(False);
     end;
     NewLine;
  end;
end;

procedure TSutraInputWriter.WriteDataSet11C(ARegion: TRegionalProperty);
var
  RelativePermeabilityParameters: TRelativePermeabilityParameters;
  Index: Integer;
  Value: Double;
  PestParam: string;
begin
  if (FOptions.TransportChoice = tcFreezing)
    or (FOptions.SaturationChoice = scUnsaturated) then
  begin
    RelativePermeabilityParameters := ARegion.RelativePermeabilityParameters;
    case RelativePermeabilityParameters.RelativePermeabilityChoice of
      rpcNone:
        begin
          WriteString('''NONE'' ');
          WriteString(' # DATASET 11C: RKMOD');
        end;
      rpcVanGenuchten:
        begin
          WriteString('''VGEN'' ');

          Value := EvaluateFormula(RelativePermeabilityParameters.RelativePermParam,
            'DATASET 11C: van Genuchten function parameter n_VG (VN)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

          Value := EvaluateFormula(RelativePermeabilityParameters.MinRelativePerm,
            'DATASET 11C: Minimum relative permeability (RKMIN)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
             Value, -1, -1, -1);

//          WriteFloat(RelativePermeabilityParameters.RelativePermParam);
//          WriteFloat(RelativePermeabilityParameters.MinRelativePerm);
          WriteString(' # DATASET 11C: RKMOD, VN, RKMIN');
        end;
      rpcBrooksCorey:
        begin
          WriteString('''BCOR'' ');

          Value := EvaluateFormula(RelativePermeabilityParameters.PoreSizeDistributionIndex,
            'DATASET 11C: Pore size distribution index (RLAMB)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

          Value := EvaluateFormula(RelativePermeabilityParameters.MinRelativePerm,
            'DATASET 11C: Minimum relative permeability (RKMIN)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

//          WriteFloat(RelativePermeabilityParameters.PoreSizeDistributionIndex);
//          WriteFloat(RelativePermeabilityParameters.MinRelativePerm);
          WriteString(' # DATASET 11C: RKMOD, RLAMB, RKMIN');
        end;
      rpcPiecewiseLinear:
        begin
          WriteString('''PLIN'' ');

          Value := EvaluateFormula(RelativePermeabilityParameters.WaterSaturationAtMinPermeability,
            'DATASET 11C: Saturation at minimum permeability (SLRKMIN)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);


          Value := EvaluateFormula(RelativePermeabilityParameters.MinRelativePerm,
            'DATASET 11C: Minimum relative permeability (RKMIN)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

//          WriteFloat(RelativePermeabilityParameters.MinRelativePerm);
          WriteString(' # DATASET 11C: RKMOD, SLRKMIN, RKMIN');
        end;
      rpcUserDefined:
        begin
          WriteString('''UDEF'' ');
          WriteInteger(RelativePermeabilityParameters.FunctionParameters.Count);
          for Index := 0 to RelativePermeabilityParameters.FunctionParameters.Count - 1 do
          begin
            WriteFloat(RelativePermeabilityParameters.FunctionParameters[Index].Value);
          end;
          WriteString(' # DATASET 11C: RKMOD NRKPAR RKPAR(0..N)');
        end;
      else
        Assert(False);
    end;
    NewLine;
  end;
end;

procedure TSutraInputWriter.WriteDataSet11D(ARegion: TRegionalProperty);
var
  LiquidWaterSaturationParameters: TLiquidWaterSaturationParameters;
  Index: Integer;
  Value: Double;
  PestParam: string;
begin
  if (FOptions.TransportChoice = tcFreezing) then
  begin
    LiquidWaterSaturationParameters := ARegion.LiquidWaterSaturationParameters;
    case LiquidWaterSaturationParameters.LiquidWaterSaturationChoice of
      lwscNone:
        begin
          WriteString('''NONE'' ');
          WriteString(' # DATASET 11D: SLMOD');
        end;
      lwscExponential:
        begin
          WriteString('''EXPO'' ');

          Value := EvaluateFormula(LiquidWaterSaturationParameters.ResidualLiquidWaterSaturation,
            'DATASET 11D: Residual liquid water saturation  (SLSATRES)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

          Value := EvaluateFormula(LiquidWaterSaturationParameters.ExponentialParameter,
            'DATASET 11D: Exponential parameter w_EXP (W)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

//          WriteFloat(LiquidWaterSaturationParameters.ResidualLiquidWaterSaturation);
//          WriteFloat(LiquidWaterSaturationParameters.ExponentialParameter);
          WriteString(' # DATASET 11D: SLMOD, SLSATRES, W');
        end;
      lwscPowerLaw:
        begin
          WriteString('''POWR'' ');

          Value := EvaluateFormula(LiquidWaterSaturationParameters.ResidualLiquidWaterSaturation,
            'DATASET 11D: Residual liquid water saturation  (SLSATRES)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

          Value := EvaluateFormula(LiquidWaterSaturationParameters.PowerLawAlpha,
            'DATASET 11D: Modified power law model parameter, alpha_POW (ALPHA)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

          Value := EvaluateFormula(LiquidWaterSaturationParameters.PowerLawBeta,
            'DATASET 11D: Modified power law model parameter, Beta_POW(BETA)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

//          WriteFloat(LiquidWaterSaturationParameters.ResidualLiquidWaterSaturation);
//          WriteFloat(LiquidWaterSaturationParameters.PowerLawAlpha);
//          WriteFloat(LiquidWaterSaturationParameters.PowerLawBeta);
          WriteString(' # DATASET 11D: SLMOD, SLSATRES, W');
        end;
      lwscPiecewiseLinear:
        begin
          WriteString('''PLIN'' ');

          Value := EvaluateFormula(LiquidWaterSaturationParameters.ResidualLiquidWaterSaturation,
            'DATASET 11D: Residual liquid water saturation  (SLSATRES)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

          Value := EvaluateFormula(LiquidWaterSaturationParameters.TempAtResidualLiquidWaterSaturation,
            'DATASET 11D: Relative temperature at which the liquid saturation reaches the residual liquid saturation (TLRES)', PestParam);
          WriteFormulaOrValueBasedOnAPestName(PestParam,
            Value, -1, -1, -1);

//          WriteFloat(LiquidWaterSaturationParameters.ResidualLiquidWaterSaturation);
//          WriteFloat(LiquidWaterSaturationParameters.TempAtResidualLiquidWaterSaturation);
          WriteString(' # DATASET 11D: SLMOD, SLSATRES, TLRES');
        end;
      lwscUserDefined:
        begin
          WriteString('''UDEF'' ');
          WriteInteger(LiquidWaterSaturationParameters.FunctionParameters.Count);
          for Index := 0 to LiquidWaterSaturationParameters.FunctionParameters.Count - 1 do
          begin
            WriteFloat(LiquidWaterSaturationParameters.FunctionParameters[Index].Value);
          end;
          WriteString(' # DATASET 11D: SLMOD NRKPAR SLPAR(0..N)');
        end;
      else
        Assert(False);
    end;
    NewLine;
  end;
end;

procedure TSutraInputWriter.WriteDataSet11E(ARegion: TRegionalProperty);
var
  FreezingTempAndLatentHeat: TFreezingTempAndLatentHeat;
  Value: Double;
  PestParam: string;
begin
  if (FOptions.TransportChoice = tcFreezing) then
  begin
    FreezingTempAndLatentHeat := ARegion.FreezingTempAndLatentHeat;

    Value := EvaluateFormula(FreezingTempAndLatentHeat.MaxFreezePoreWaterTemperature,
      'DATASET 11E: Maximum freezing temperature of pore water (TFREEZ)', PestParam);
    WriteFormulaOrValueBasedOnAPestName(PestParam,
      Value, -1, -1, -1);

    Value := EvaluateFormula(FreezingTempAndLatentHeat.LatentHeatOfFusion,
      'DATASET 11E: Latent heat of fusion (HTLAT)', PestParam);
    WriteFormulaOrValueBasedOnAPestName(PestParam,
      Value, -1, -1, -1);

//    WriteFloat(FreezingTempAndLatentHeat.MaxFreezePoreWaterTemperature);
//    WriteFloat(FreezingTempAndLatentHeat.LatentHeatOfFusion);
    WriteString(' # DATASET 11DE: TFREEZ, HTLAT');
    NewLine;
  end;
end;

procedure TSutraInputWriter.WriteDataSet12;
var
  PRODFØ: Double;
  PRODSØ: Double;
  PRODF1: Double;
  PRODS1: Double;
begin
  if Model.ModelSelection = msSutra40 then
  begin
    Exit;
  end;
  WriteCommentLine('Data set 12');
  PRODFØ := FOptions.ZeroFluidProduction;
  PRODSØ := FOptions.ZeroImmobileProduction;
  PRODF1 := FOptions.FirstFluidProduction;
  PRODS1 := FOptions.FirstImmobileProduction;
  WriteFloat(PRODFØ);
  WriteFloat(PRODSØ);
  WriteFloat(PRODF1);
  WriteFloat(PRODS1);
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet13;
var
  GRAVX: Double;
  GRAVY: Double;
  GRAVZ: Double;
begin
  GRAVX := 0;
  GRAVY := 0;
  GRAVZ := 0;
  WriteCommentLine('Data set 13');
  case FOptions.TransportChoice of
    tcSolute, tcEnergy, tcFreezing:
      begin
        GRAVX := FOptions.GravityX;
        GRAVY := FOptions.GravityY;
        if FMesh.MeshType in [mt2D, mtProfile] then
        begin
          GRAVZ := 0;
        end
        else
        begin
          GRAVZ := FOptions.GravityZ;
        end;
      end;
    tcSoluteHead:
      begin
        GRAVX := 0;
        GRAVY := 0;
        GRAVZ := 0;
      end;
    else
      Assert(False);
  end;
  WriteFloat(GRAVX);
  WriteFloat(GRAVY);
  WriteFloat(GRAVZ);
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet14A;
const
  SCALX = 1.;
  SCALY = 1.;
  SCALZ = 1.;
  PORFAC = 1.;
  COMPMAF = 1.;
  CSF = 1.;
  RHOSF = 1.;
  PRODLØF = 1.;
  PRODSØF = 1.;
  PRODL1F = 1.;
  PRODS1F = 1.;
  PRODI0F = 1.;
var
  Comment: string;
begin
  WriteCommentLine('Data set 14A');
  Comment := 'NODE   SCALX                 SCALY                 SCALZ                 PORFAC             ';
  if Model.DoSutra4Used(nil) then
  begin
    Comment := Comment + '   COMPMAF            ';
    Comment := Comment + '   CSF                ';
    Comment := Comment + '   RHOSF              ';
    if Model.DoSutra4ProductionUsed(nil) then
    begin
      Comment := Comment + '    PROD            ';
      Comment := Comment + '   PRODL0F            ';
      Comment := Comment + '   PRODS0F            ';
    end
    else
    begin
      Comment := Comment + '    NOPROD          ';
    end;
    if Model.DoSutra4SoluteUsed(nil) then
    begin
      Comment := Comment + '   PRODL1F            ';
      Comment := Comment + '   PRODS1F            ';
    end;
    if Model.DoSutra4FreezingUsed(nil) then
    begin
      Comment := Comment + '   PRODI0F            ';
    end;
  end;
  WriteCommentLine(Comment);

  WriteString('''NODE'' ');
  WriteFloat(SCALX);
  WriteFloat(SCALY);
  WriteFloat(SCALZ);
  WriteFloat(PORFAC);

  if Model.DoSutra4Used(nil) then
  begin
    WriteFloat(COMPMAF);
    if Model.DoSutra4EnergyUsed(nil) then
    begin
      WriteFloat(CSF);
    end
    else
    begin
      WriteFloat(0);
    end;
    WriteFloat(RHOSF);
    if Model.DoSutra4ProductionUsed(nil) then
    begin
      WriteString('   ''PROD''           ');
      WriteFloat(PRODLØF);
      WriteFloat(PRODSØF);
    end
    else
    begin
      WriteString('   ''NOPROD''         ');
    end;
    if Model.DoSutra4SoluteUsed(nil) then
    begin
      WriteFloat(PRODL1F);
      WriteFloat(PRODS1F);
    end;
    if Model.DoSutra4FreezingUsed(nil) then
    begin
      WriteFloat(PRODI0F);
    end;
  end;
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet14B;
var
  Porosity: TDataArray;
  UnsatRegion: TDataArray;
  Thickness: TDataArray;
  Nodes: TNodeDataList;
  LayerIndex: Integer;
  NodeIndex: Integer;
  ANode2D: TSutraNode2D;
  NodeData: TNodeData;
  ANode3D: TSutraNode3D;
  TempFileName: string;
  PestParametersUsed: Boolean;
  DataFileWriter: TSutraNodeDataWriter;
  Sutra14BWriter: TSutraData14BScriptWriter;
  TempFile: string;
  ParameterZoneWriter: TParameterZoneWriter;
  TempFileRoot: string;
  COMPMA: TDataArray;
  CS: TDataArray;
  RHOS: TDataArray;
  PRODL0: TDataArray;
  PRODS0: TDataArray;
  PRODL1: TDataArray;
  PRODS1: TDataArray;
  PRODI: TDataArray;
  Sutra4Used: Boolean;
  Sutra4EnergyUsed: Boolean;
  Sutra4SoluteUsed: Boolean;
  Sutra4EnergyOrSorptionUsed: Boolean;
  Sutra4FreezingUsed: Boolean;
  Sutra4ProductionUsed: Boolean;
  procedure ExportDataForPest(ADataArray: TDataArray);
  begin
    if Model.PestUsed then
    begin
      DataFileWriter := TSutraNodeDataWriter.Create(Model, etExport);
      try
        DataFileWriter.WriteFile(FFileName, ADataArray);
      finally
        DataFileWriter.Free;
      end;
      if ADataArray.PestParametersUsed and not WritingTemplate then
      begin
        PestParametersUsed := True;
        // Create an array file too in case the data set is used in
        // a boundary condition.
        ParameterZoneWriter := TParameterZoneWriter.Create(Model, etExport);
        try
          TempFile := ChangeFileExt(FNameOfFile, '');
          ParameterZoneWriter.WriteFile(TempFile, ADataArray, ADataArray.Name);
        finally
          ParameterZoneWriter.Free;
        end;
      end;
    end;
  end;
  procedure Write14BInternal(Layer: Integer);
  var
    NodeIndex: Integer;
    Comment: string;
  begin
    if Layer <= 1 then
    begin
      WriteCommentLine('Data set 14B');
      Comment := '  II  NREG  X                     Y                     Z                     POR                ';
      if Model.DoSutra4Used(nil) then
      begin
        Comment := Comment + '   COMPMA             ';
        Comment := Comment + '   CS                 ';
        Comment := Comment + '   RHOS               ';
        if Model.DoSutra4ProductionUsed(nil) then
        begin
          Comment := Comment + '   PRODL0             ';
          Comment := Comment + '   PRODS0             ';
        end;
        if Model.DoSutra4SoluteUsed(nil) then
        begin
          Comment := Comment + '   PRODL1             ';
          Comment := Comment + '   PRODS1             ';
        end;
        if Model.DoSutra4FreezingUsed(nil) then
        begin
          Comment := Comment + '   PRODI              ';
        end;
      end;
      WriteCommentLine(Comment);
    end;
    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      NodeData := Nodes[NodeIndex];
      if (NodeData.Layer = Layer) or (Layer < 0) then
      begin
        Assert(NodeIndex = NodeData.Number);
        WriteInteger(NodeData.Number + 1);
        WriteInteger(NodeData.NREG);
        WriteFloat(NodeData.X);
        WriteFloat(NodeData.Y);
        WriteFloat(NodeData.Z);
        WriteFloat(NodeData.Porosity);

        if Sutra4Used then
        begin
          WriteFloat(NodeData.COMPMA);
          WriteFloat(NodeData.CS);
          WriteFloat(NodeData.RHOS);
          if Sutra4ProductionUsed then
          begin
            WriteFloat(NodeData.PRODL0);
            WriteFloat(NodeData.PRODS0);
          end;
          if Sutra4SoluteUsed then
          begin
            WriteFloat(NodeData.PRODL1);
            WriteFloat(NodeData.PRODS1);
          end;
          if Sutra4FreezingUsed then
          begin
            WriteFloat(NodeData.PRODI);
          end;
        end;

        NewLine;
      end;
    end;
  end;
begin
  Sutra4Used := Model.DoSutra4Used(nil);
  Sutra4EnergyUsed := Model.DoSutra4EnergyUsed(nil);
  Sutra4SoluteUsed := Model.DoSutra4SoluteUsed(nil);
  Sutra4EnergyOrSorptionUsed := Model.DoSutra4EnergyOrSorptionUsed(nil);
  Sutra4FreezingUsed := Model.DoSutra4FreezingUsed(nil);
  Sutra4ProductionUsed := Model.DoSutra4ProductionUsed(nil);
  PestParametersUsed := False;
  Porosity := Model.DataArrayManager.GetDataSetByName(KNodalPorosity);
  Porosity.Initialize;
  ExportDataForPest(Porosity);

  COMPMA := nil;
  if Sutra4Used then
  begin
    COMPMA := Model.DataArrayManager.GetDataSetByName(KSolidMatrixComp);
    COMPMA.Initialize;
    ExportDataForPest(COMPMA);
  end;
  CS := nil;
  if Sutra4EnergyUsed then
  begin
    CS := Model.DataArrayManager.GetDataSetByName(KSolidGrainSpecificHeat);
    CS.Initialize;
    ExportDataForPest(CS);
  end;
  RHOS := nil;
  if Sutra4EnergyOrSorptionUsed then
  begin
    RHOS := Model.DataArrayManager.GetDataSetByName(KSolidGrainDensity);
    RHOS.Initialize;
    ExportDataForPest(RHOS);
  end;
  PRODL0 := nil;
  PRODS0 := nil;
  if Sutra4ProductionUsed then
  begin
    PRODL0 := Model.DataArrayManager.GetDataSetByName(KZeroOrderProductionRateInLiquid);
    PRODL0.Initialize;
    ExportDataForPest(PRODL0);
    PRODS0 := Model.DataArrayManager.GetDataSetByName(KZeroOrderProductionRateInImmobile);
    PRODS0.Initialize;
    ExportDataForPest(PRODS0);
  end;
  PRODL1 := nil;
  PRODS1 := nil;
  if Sutra4SoluteUsed then
  begin
    PRODL1 := Model.DataArrayManager.GetDataSetByName(KFirstOrderProductionRateInLiquid);
    PRODL1.Initialize;
    ExportDataForPest(PRODL1);
    PRODS1 := Model.DataArrayManager.GetDataSetByName(KFirstOrderProductionRateInImmobile);
    PRODS1.Initialize;
    ExportDataForPest(PRODS1);
  end;
  PRODI := nil;
  if Sutra4FreezingUsed then
  begin
    PRODI := Model.DataArrayManager.GetDataSetByName(KZeroOrderProductionRateInIce);
    PRODI.Initialize;
    ExportDataForPest(PRODI);
  end;

//  if Model.PestUsed then
//  begin
//    DataFileWriter := TSutraNodeDataWriter.Create(Model, etExport);
//    try
//      DataFileWriter.WriteFile(FFileName, Porosity);
//    finally
//      DataFileWriter.Free;
//    end;
//    if Porosity.PestParametersUsed and not WritingTemplate then
//    begin
//      PestParametersUsed := True;
//      // Create an array file too in case the data set is used in
//      // a boundary condition.
//      ParameterZoneWriter := TParameterZoneWriter.Create(Model, etExport);
//      try
//        TempFile := ChangeFileExt(FNameOfFile, '');
//        ParameterZoneWriter.WriteFile(TempFile, Porosity, Porosity.Name);
//      finally
//        ParameterZoneWriter.Free;
//      end;
//    end;
//  end;

  if FMesh.MeshType in [mt2D, mtProfile] then
  begin
    Thickness := Model.DataArrayManager.GetDataSetByName(KNodalThickness);
    Thickness.Initialize;
    ExportDataForPest(Thickness);
//    if Model.PestUsed then
//    begin
//      DataFileWriter := TSutraNodeDataWriter.Create(Model, etExport);
//      try
//        DataFileWriter.WriteFile(FFileName, Thickness);
//      finally
//        DataFileWriter.Free;
//      end;
//      if Thickness.PestParametersUsed and not WritingTemplate then
//      begin
//        PestParametersUsed := True;
//
//
//        // Create an array file too in case the data set is used in
//        // a boundary condition.
//        ParameterZoneWriter := TParameterZoneWriter.Create(Model, etExport);
//        try
//          TempFile := ChangeFileExt(FNameOfFile, '');
//          ParameterZoneWriter.WriteFile(TempFile, Thickness, Thickness.Name);
//        finally
//          ParameterZoneWriter.Free;
//        end;
//      end;
//    end;
  end
  else
  begin
    Thickness := nil;
  end;

  if Model.DoSutraUnsatRegionUsed(nil) then
  begin
    UnsatRegion := Model.DataArrayManager.GetDataSetByName(KUnsatRegionNodes);
    UnsatRegion.Initialize;
    if PestParametersUsed then
    begin
      DataFileWriter := TSutraNodeDataWriter.Create(Model, etExport);
      try
        DataFileWriter.WriteFile(FFileName, UnsatRegion);
      finally
        DataFileWriter.Free;
      end;
    end;
  end
  else
  begin
    UnsatRegion := nil;
    if PestParametersUsed then
    begin
      DataFileWriter := TSutraNodeDataWriter.Create(Model, etExport);
      try
        DataFileWriter.WriteFile(FFileName, KUnsatRegionNodes, rdtInteger);
      finally
        DataFileWriter.Free;
      end;
    end;
  end;

  Nodes := TNodeDataList.Create;
  try
    if FMesh.MeshType in [mt2D, mtProfile] then
    begin
      Nodes.Capacity := FMesh.Mesh2D.Nodes.Count;
      for NodeIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
      begin
        ANode2D := FMesh.Mesh2D.Nodes[NodeIndex];
        NodeData := TNodeData.Create;
        Nodes.Add(NodeData);
        NodeData.Number := ANode2D.Number;
        if UnsatRegion = nil then
        begin
          NodeData.NREG := 0;
        end
        else
        begin
          NodeData.NREG := UnsatRegion.IntegerData[0,0,NodeIndex];
        end;
        NodeData.X := ANode2D.X;
        NodeData.Y := ANode2D.Y;
        NodeData.Z := Thickness.RealData[0,0,NodeIndex];
        NodeData.Porosity := Porosity.RealData[0,0,NodeIndex];
        if Sutra4Used then
        begin
          NodeData.COMPMA := COMPMA.RealData[0,0,NodeIndex];
        end;
        if Sutra4EnergyUsed then
        begin
          NodeData.CS := CS.RealData[0,0,NodeIndex];
        end
        else
        begin
          NodeData.CS := 0;
        end;
        if Sutra4EnergyOrSorptionUsed then
        begin
          NodeData.RHOS := RHOS.RealData[0,0,NodeIndex];
        end
        else
        begin
          NodeData.RHOS := 0;
        end;
        if Sutra4ProductionUsed then
        begin
          NodeData.PRODL0 := PRODL0.RealData[0,0,NodeIndex];
          NodeData.PRODS0 := PRODS0.RealData[0,0,NodeIndex];
        end;
        if Sutra4SoluteUsed then
        begin
          NodeData.PRODL1 := PRODL1.RealData[0,0,NodeIndex];
          NodeData.PRODS1 := PRODS1.RealData[0,0,NodeIndex];
        end;
        if Sutra4FreezingUsed then
        begin
          NodeData.PRODI := PRODI.RealData[0,0,NodeIndex];
        end;
      end;
    end
    else
    begin
      Nodes.Capacity := FMesh.ActiveNodeCount;
      for LayerIndex := 0 to FMesh.LayerCount do
      begin
        for NodeIndex := 0 to FMesh.Mesh2D.Nodes.Count - 1 do
        begin
          ANode3D := FMesh.NodeArray[LayerIndex,NodeIndex];
          if ANode3D.Active then
          begin
            NodeData := TNodeData.Create;
            Nodes.Add(NodeData);
            NodeData.Layer := LayerIndex+1;
            NodeData.Number := ANode3D.Number;
            if UnsatRegion = nil then
            begin
              NodeData.NREG := 0;
            end
            else
            begin
              NodeData.NREG := UnsatRegion.IntegerData[LayerIndex,0,NodeIndex];
            end;
            NodeData.X := ANode3D.X;
            NodeData.Y := ANode3D.Y;
            NodeData.Z := ANode3D.Z;
            NodeData.Porosity := Porosity.RealData[LayerIndex,0,NodeIndex];
            if Sutra4Used then
            begin
              NodeData.COMPMA := COMPMA.RealData[LayerIndex,0,NodeIndex];
            end;
            if Sutra4EnergyUsed then
            begin
              NodeData.CS := CS.RealData[LayerIndex,0,NodeIndex];
            end
            else
            begin
              NodeData.CS := 0;
            end;
            if Sutra4EnergyOrSorptionUsed then
            begin
              NodeData.RHOS := RHOS.RealData[LayerIndex,0,NodeIndex];
            end
            else
            begin
              NodeData.RHOS := 0;
            end;
            if Sutra4ProductionUsed then
            begin
              NodeData.PRODL0 := PRODL0.RealData[LayerIndex,0,NodeIndex];
              NodeData.PRODS0 := PRODS0.RealData[LayerIndex,0,NodeIndex];
            end;
            if Sutra4SoluteUsed then
            begin
              NodeData.PRODL1 := PRODL1.RealData[LayerIndex,0,NodeIndex];
              NodeData.PRODS1 := PRODS1.RealData[LayerIndex,0,NodeIndex];
            end;
            if Sutra4FreezingUsed then
            begin
              NodeData.PRODI := PRODI.RealData[LayerIndex,0,NodeIndex];
            end;
          end;
        end;
      end;

      Nodes.Sort(TNodeDataComparer.Construct(
        function (const L, R: TNodeData): integer
        begin
          result := L.Number - R.Number;
        end));
    end;

    if PestParametersUsed then
    begin
      TempFileName := ChangeFileExt(FFileName, '.14B');
      WriteString('@INSERT 99 ');
      WriteString(ExtractFileName(TempFileName));
      if FMesh.MeshType <> mt3D then
      begin
        Model.FilesToDelete.Add(TempFileName);
      end;
      NewLine;
      if not WritingTemplate then
      begin
        Sutra14BWriter := TSutraData14BScriptWriter.Create(Model, etExport);
        try
          Sutra14BWriter.WriteFiles(FFileName);
        finally
          Sutra14BWriter.Free;
        end;
      end;
      if FMesh.MeshType = mt3D then
      begin
        OpenTempFile(TempFileName);
        try
          TempFileRoot := TempFileName + '_';
          TempFileName := ExtractFileName(TempFileRoot);
          for LayerIndex := 1 to FMesh.LayerCount+1 do
          begin
            WriteString('@INSERT 99 ');
            WriteString(TempFileName + IntToStr(LayerIndex));
            Model.FilesToDelete.Add(TempFileName + IntToStr(LayerIndex));
            NewLine;

            OpenTempFile(TempFileRoot + IntToStr(LayerIndex));
            try
              Write14BInternal(LayerIndex);
            finally
              CloseTempFile;
            end;
          end;
        finally
          CloseTempFile;
        end;
      end
      else
      begin
        OpenTempFile(TempFileName);
        try
          Write14BInternal(0);
        finally
          CloseTempFile;
        end;
      end;
    end
    else
    begin
      Write14BInternal(-1);
    end;
  finally
    Nodes.Free;
  end;

  Model.DataArrayManager.AddDataSetToCache(Porosity);
  if UnsatRegion <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(UnsatRegion);
  end;
  if Thickness <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(Thickness);
  end;

  if COMPMA <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(COMPMA);
  end;
  if CS <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(CS);
  end;
  if RHOS <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(RHOS);
  end;
  if PRODL0 <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(PRODL0);
  end;
  if PRODS0 <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(PRODS0);
  end;
  if PRODL1 <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(PRODL1);
  end;
  if PRODS1 <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(PRODS1);
  end;
  if PRODI <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(PRODI);
  end;

  Model.DataArrayManager.CacheDataArrays;
end;

procedure TSutraInputWriter.WriteDataSet15A;
const
  PMAXFA = 1.;
  PMIDFA = 1.;
  PMINFA = 1.;
  ANG1FA = 1.;
  ANG2FA = 1.;
  ANG3FA = 1.;
  ALMAXF = 1.;
  ALMIDF = 1.;
  ALMINF = 1.;
  ATMAXF = 1.;
  ATMIDF = 1.;
  ATMINF = 1.;
  SIGMASF = 1.;
  SIGMAAF = 1.;
var
  Comment: string;
begin
  WriteCommentLine('Data set 15A');
  Comment := 'ELEMENT   PMAXFA             ';
  if FMesh.MeshType = mt3D then
  begin
    Comment := Comment + '   PMIDFA             ';
  end;
  Comment := Comment + '   PMINFA             ';
  Comment := Comment + '   ANG1FA             ';
  if FMesh.MeshType = mt3D then
  begin
    Comment := Comment + '   ANG2FA             ';
    Comment := Comment + '   ANG3FA             ';
  end;
  Comment := Comment + '   ALMAXF             ';
  if FMesh.MeshType = mt3D then
  begin
    Comment := Comment + '   ALMIDF             ';
  end;
  Comment := Comment + '   ALMINF             ';
  Comment := Comment + '   ATMAXF             ';
  if FMesh.MeshType = mt3D then
  begin
    Comment := Comment + '   ATMIDF             ';
  end;
  Comment := Comment + '   ATMINF             ';

  if Model.DoSutra4EnergyUsed(nil) then
  begin
    Comment := Comment + '   SIGMASF            ';
    Comment := Comment + '   SIGMAAF            ';
  end;
  WriteCommentLine(Comment);

  WriteString('''ELEMENT'' ');
  WriteFloat(PMAXFA);
  if FMesh.MeshType = mt3D then
  begin
    WriteFloat(PMIDFA);
  end;
  WriteFloat(PMINFA);
  WriteFloat(ANG1FA);
  if FMesh.MeshType = mt3D then
  begin
    WriteFloat(ANG2FA);
    WriteFloat(ANG3FA);
  end;
  WriteFloat(ALMAXF);
  if FMesh.MeshType = mt3D then
  begin
    WriteFloat(ALMIDF);
  end;
  WriteFloat(ALMINF);
  WriteFloat(ATMAXF);
  if FMesh.MeshType = mt3D then
  begin
    WriteFloat(ATMIDF);
  end;
  WriteFloat(ATMINF);

  if Model.DoSutra4EnergyUsed(nil) then
  begin
    WriteFloat(SIGMASF);
    WriteFloat(SIGMAAF);
  end;
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet15B;
var
  UnsatRegion: TDataArray;
  MaxPerm: TDataArray;
  MinPerm: TDataArray;
  MidPerm: TDataArray;
  HorizAngle: TDataArray;
  VerticalAngle: TDataArray;
  RotationAngle: TDataArray;
  MaxLongDisp: TDataArray;
  MidLongDisp: TDataArray;
  MinLongDisp: TDataArray;
  MaxTransvDisp: TDataArray;
  MidTransvDisp: TDataArray;
  MinTransvDisp: TDataArray;
  ElementList: TElementDataList;
  ElementIndex: Integer;
  AnElement2D: TSutraElement2D;
  ElData: TElementData;
  LayerIndex: Integer;
  AnElement3D: TSutraElement3D;
  TempFileName: string;
  PestParametersUsed: Boolean;
  DataFileWriter: TSutraElementDataWriter;
  Sutra15BWriter: TSutraData15BScriptWriter;
  TempFileRoot: string;
  Sutra4EnergyUsed: Boolean;
  SIGMAS: TDataArray;
  SIGMAA: TDataArray;
  procedure ExportDataForPest(DataArray: TDataArray);
  begin
    if Model.PestUsed then
    begin
      DataFileWriter := TSutraElementDataWriter.Create(Model, etExport);
      try
        DataFileWriter.WriteFile(FFileName, DataArray);
      finally
        DataFileWriter.Free;
      end;
      if DataArray.PestParametersUsed then
      begin
        PestParametersUsed := True;
      end;
    end;
  end;
  procedure InternalWrite15B(Layer: Integer);
  var
    ElementIndex: Integer;
    Comment: string;
  begin
    if Layer <= 1 then
    begin
      WriteCommentLine('Data set 15B');
      Comment := '   L  LREG  PMAX                  ';
      if FMesh.MeshType = mt3D then
      begin
        Comment := Comment + 'PMID                  ';
      end;
      Comment := Comment + 'PMIN                  ';
      Comment := Comment + 'ANGLE1                ';
      if FMesh.MeshType = mt3D then
      begin
        Comment := Comment + 'ANGLE2                ';
        Comment := Comment + 'ANGLE3                ';
      end;
      Comment := Comment + 'ALMAX                 ';
      if FMesh.MeshType = mt3D then
      begin
        Comment := Comment + 'ALMID                 ';
      end;
      Comment := Comment + 'ALMIN                 ';
      Comment := Comment + 'ATMAX                 ';
      if FMesh.MeshType = mt3D then
      begin
        Comment := Comment + 'ATMID                 ';
      end;
      Comment := Comment + 'ATMIN                 ';
      if Sutra4EnergyUsed then
      begin
        Comment := Comment + 'SIGMAS                ';
        Comment := Comment + 'SIGMAA                ';
      end;
      WriteCommentLine(Comment);
    end;
    for ElementIndex := 0 to ElementList.Count - 1 do
    begin
      ElData := ElementList[ElementIndex];
      if (ElData.Layer = Layer) or (Layer <= 0) then
      begin
        Assert(ElData.Number = ElementIndex);
        WriteInteger(ElData.Number+1);
        WriteInteger(ElData.LREG);
        WriteFloat(ElData.PMAX);
        if FMesh.MeshType = mt3D then
        begin
          WriteFloat(ElData.PMID);
        end;
        WriteFloat(ElData.PMIN);
        WriteFloat(ElData.ANGLE1);
        if FMesh.MeshType = mt3D then
        begin
          WriteFloat(ElData.ANGLE2);
          WriteFloat(ElData.ANGLE3);
        end;
        WriteFloat(ElData.ALMAX);
        if FMesh.MeshType = mt3D then
        begin
          WriteFloat(ElData.ALMID);
        end;
        WriteFloat(ElData.ALMIN);
        WriteFloat(ElData.ATMAX);
        if FMesh.MeshType = mt3D then
        begin
          WriteFloat(ElData.ATMID);
        end;
        WriteFloat(ElData.ATMIN);
        if Sutra4EnergyUsed then
        begin
          WriteFloat(ElData.SIGMAS);
          WriteFloat(ElData.SIGMAA);
        end;
        NewLine;
        case FMesh.MeshType of
          mt2D, mtProfile:
            begin
              if ElData.PMAX < ElData.PMIN then
              begin
                case FOptions.TransportChoice of
                  tcSolute, tcEnergy, tcFreezing:
                    begin
                      frmErrorsAndWarnings.AddWarning(Model,
                        StrMaxPermMinPerm, IntToStr(ElData.Number+1));
                    end;
                  tcSoluteHead:
                    begin
                      frmErrorsAndWarnings.AddWarning(Model,
                        StrMaxKMinK, IntToStr(ElData.Number+1));
                    end;
                  else Assert(False);
                end;
              end;
            end;
          mt3D:
            begin
              if ElData.PMAX < ElData.PMID then
              begin
                case FOptions.TransportChoice of
                  tcSolute, tcEnergy, tcFreezing:
                    begin
                      frmErrorsAndWarnings.AddWarning(Model,
                        StrMaxPermMidPerm, IntToStr(ElData.Number+1));
                    end;
                  tcSoluteHead:
                    begin
                      frmErrorsAndWarnings.AddWarning(Model,
                        StrMaxKMidK, IntToStr(ElData.Number+1));
                    end;
                  else Assert(False);
                end;
              end;
              if ElData.PMID < ElData.PMIN then
              begin
                case FOptions.TransportChoice of
                  tcSolute, tcEnergy, tcFreezing:
                    begin
                      frmErrorsAndWarnings.AddWarning(Model,
                        StrMidPermMinPerm, IntToStr(ElData.Number+1));
                    end;
                  tcSoluteHead:
                    begin
                      frmErrorsAndWarnings.AddWarning(Model,
                        StrMidKMinK, IntToStr(ElData.Number+1));
                    end;
                  else Assert(False);
                end;
              end;
            end;
        end;
      end;
    end
  end;
begin
  Sutra4EnergyUsed := Model.DoSutra4EnergyUsed(nil);
  PestParametersUsed := False;
  MaxPerm := nil;
  case FOptions.TransportChoice of
    tcSolute, tcEnergy, tcFreezing:
      MaxPerm := Model.DataArrayManager.GetDataSetByName(KMaximumPermeability);
    tcSoluteHead:
      MaxPerm := Model.DataArrayManager.GetDataSetByName(KMaximumK);
    else Assert(False);
  end;
  MaxPerm.Initialize;
  ExportDataForPest(MaxPerm);

  MidPerm := nil;
  if FMesh.MeshType = mt3D then
  begin
    case FOptions.TransportChoice of
      tcSolute, tcEnergy, tcFreezing:
        MidPerm := Model.DataArrayManager.GetDataSetByName(KMiddlePermeability);
      tcSoluteHead:
        MidPerm := Model.DataArrayManager.GetDataSetByName(KMiddleK);
      else Assert(False);
    end;
    MidPerm.Initialize;
    ExportDataForPest(MidPerm);
  end
  else
  begin
    MidPerm := nil;
  end;

  MinPerm := nil;
  case FOptions.TransportChoice of
    tcSolute, tcEnergy, tcFreezing:
      MinPerm := Model.DataArrayManager.GetDataSetByName(KMinimumPermeability);
    tcSoluteHead:
      MinPerm := Model.DataArrayManager.GetDataSetByName(KMinimumK);
    else Assert(False);
  end;
  MinPerm.Initialize;
  ExportDataForPest(MinPerm);


  HorizAngle := Model.DataArrayManager.GetDataSetByName(KHorizontalAngle);
  HorizAngle.Initialize;
  ExportDataForPest(HorizAngle);

  if FMesh.MeshType = mt3D then
  begin
    VerticalAngle := Model.DataArrayManager.GetDataSetByName(KVerticalAngle);
    VerticalAngle.Initialize;
    ExportDataForPest(VerticalAngle);

    RotationAngle := Model.DataArrayManager.GetDataSetByName(KRotationalAngle);
    RotationAngle.Initialize;
    ExportDataForPest(RotationAngle);
  end
  else
  begin
    VerticalAngle := nil;
    RotationAngle := nil;
  end;

  MaxLongDisp := Model.DataArrayManager.GetDataSetByName(KMaxLongitudinalDisp);
  MaxLongDisp.Initialize;
  ExportDataForPest(MaxLongDisp);

//  MidLongDispParamArray := nil;
  if FMesh.MeshType = mt3D then
  begin
    MidLongDisp := Model.DataArrayManager.GetDataSetByName(KMidLongitudinalDisp);
    MidLongDisp.Initialize;
    ExportDataForPest(MidLongDisp);
  end
  else
  begin
    MidLongDisp := nil;
  end;

  MinLongDisp := Model.DataArrayManager.GetDataSetByName(KMinLongitudinalDisp);
  MinLongDisp.Initialize;
  ExportDataForPest(MinLongDisp);

  MaxTransvDisp := Model.DataArrayManager.GetDataSetByName(KMaxTransverseDisp);
  MaxTransvDisp.Initialize;
  ExportDataForPest(MaxTransvDisp);

  if FMesh.MeshType = mt3D then
  begin
    MidTransvDisp := Model.DataArrayManager.GetDataSetByName(KMidTransverseDisp);
    MidTransvDisp.Initialize;
    ExportDataForPest(MidTransvDisp);
  end
  else
  begin
    MidTransvDisp := nil;
  end;

  MinTransvDisp := Model.DataArrayManager.GetDataSetByName(KMinTransverseDisp);
  MinTransvDisp.Initialize;
  ExportDataForPest(MinTransvDisp);

  if Sutra4EnergyUsed then
  begin
    SIGMAS := Model.DataArrayManager.GetDataSetByName(KScaledSolidGrainThermalConductivity);
    SIGMAS.Initialize;
    ExportDataForPest(SIGMAS);
  end
  else
  begin
    SIGMAS := nil;
  end;

  if Sutra4EnergyUsed then
  begin
    SIGMAA := Model.DataArrayManager.GetDataSetByName(KScaledEffectiveAirThermalConductivity);
    SIGMAA.Initialize;
    ExportDataForPest(SIGMAA);
  end
  else
  begin
    SIGMAA := nil;
  end;

  if Model.DoSutraUnsatRegionUsed(nil) then
  begin
    UnsatRegion := Model.DataArrayManager.GetDataSetByName(KUnsatRegionElements);
    UnsatRegion.Initialize;
    ExportDataForPest(UnsatRegion);
  end
  else
  begin
    UnsatRegion := nil;
    if PestParametersUsed then
    begin
      DataFileWriter := TSutraElementDataWriter.Create(Model, etExport);
      try
        DataFileWriter.WriteFile(FFileName, KUnsatRegionElements, rdtInteger);
      finally
        DataFileWriter.Free;
      end;
    end;
  end;

  ElementList := TElementDataList.Create;
  try
    if FMesh.MeshType in [mt2D, mtProfile] then
    begin
      ElementList.Capacity := FMesh.Mesh2D.Elements.Count;
      for ElementIndex := 0 to FMesh.Mesh2D.Elements.Count - 1 do
      begin
        AnElement2D := FMesh.Mesh2D.Elements[ElementIndex];
        ElData := TElementData.Create;
        ElementList.Add(ElData);
        ElData.Number := AnElement2D.ElementNumber;
        if UnsatRegion = nil then
        begin
          ElData.LREG := 0
        end
        else
        begin
          ElData.LREG := UnsatRegion.IntegerData[0,0,ElementIndex];
        end;
        ElData.PMAX := MaxPerm.RealData[0,0,ElementIndex];
        ElData.PMID := 0;
        ElData.PMIN := MinPerm.RealData[0,0,ElementIndex];
        ElData.ANGLE1 := HorizAngle.RealData[0,0,ElementIndex];
        ElData.ANGLE2 := 0;
        ElData.ANGLE3 := 0;
        ElData.ALMAX := MaxLongDisp.RealData[0,0,ElementIndex];
        ElData.ALMID := 0;
        ElData.ALMIN := MinLongDisp.RealData[0,0,ElementIndex];
        ElData.ATMAX := MaxTransvDisp.RealData[0,0,ElementIndex];
        ElData.ATMID := 0;
        ElData.ATMIN := MinTransvDisp.RealData[0,0,ElementIndex];
        if Sutra4EnergyUsed then
        begin
          ElData.SIGMAS := SIGMAS.RealData[0,0,ElementIndex];
          ElData.SIGMAA := SIGMAA.RealData[0,0,ElementIndex];
        end
        else
        begin
          ElData.SIGMAS := 0;
          ElData.SIGMAA := 0;
        end;
      end;
    end
    else
    begin
      ElementList.Capacity := FMesh.ActiveElementCount;
      for LayerIndex := 0 to FMesh.LayerCount - 1 do
      begin
        for ElementIndex := 0 to FMesh.Mesh2D.Elements.Count - 1 do
        begin
          AnElement3D := FMesh.ElementArray[LayerIndex,ElementIndex];
          if AnElement3D.Active then
          begin
            ElData := TElementData.Create;
            ElData.Layer := LayerIndex + 1;
            ElementList.Add(ElData);
            ElData.Number := AnElement3D.ElementNumber;
            if UnsatRegion = nil then
            begin
              ElData.LREG := 0
            end
            else
            begin
              ElData.LREG := UnsatRegion.IntegerData[LayerIndex,0,ElementIndex];
            end;
            ElData.PMAX := MaxPerm.RealData[LayerIndex,0,ElementIndex];
            ElData.PMID := MidPerm.RealData[LayerIndex,0,ElementIndex];
            ElData.PMIN := MinPerm.RealData[LayerIndex,0,ElementIndex];
            ElData.ANGLE1 := HorizAngle.RealData[LayerIndex,0,ElementIndex];
            ElData.ANGLE2 := VerticalAngle.RealData[LayerIndex,0,ElementIndex];
            ElData.ANGLE3 := RotationAngle.RealData[LayerIndex,0,ElementIndex];
            ElData.ALMAX := MaxLongDisp.RealData[LayerIndex,0,ElementIndex];
            ElData.ALMID := MidLongDisp.RealData[LayerIndex,0,ElementIndex];
            ElData.ALMIN := MinLongDisp.RealData[LayerIndex,0,ElementIndex];
            ElData.ATMAX := MaxTransvDisp.RealData[LayerIndex,0,ElementIndex];
            ElData.ATMID := MidTransvDisp.RealData[LayerIndex,0,ElementIndex];
            ElData.ATMIN := MinTransvDisp.RealData[LayerIndex,0,ElementIndex];
            if Sutra4EnergyUsed then
            begin
              ElData.SIGMAS := SIGMAS.RealData[LayerIndex,0,ElementIndex];
              ElData.SIGMAA := SIGMAA.RealData[LayerIndex,0,ElementIndex];
            end
            else
            begin
              ElData.SIGMAS := 0;
              ElData.SIGMAA := 0;
            end;
          end;
        end;
      end;
    end;

    ElementList.Sort(TElementDataComparer.Construct(
      function (const L, R: TElementData): integer
      begin
        result := L.Number - R.Number;
      end));

    if PestParametersUsed then
    begin
      TempFileName := ChangeFileExt(FFileName, '.15B');
      WriteString('@INSERT 99 ');
      WriteString(ExtractFileName(TempFileName));
      if FMesh.MeshType <> mt3D then
      begin
        Model.FilesToDelete.Add(TempFileName);
      end;
      NewLine;
      if not WritingTemplate then
      begin
        Sutra15BWriter := TSutraData15BScriptWriter.Create(Model, etExport);
        try
          Sutra15BWriter.WriteFiles(FFileName);
        finally
          Sutra15BWriter.Free;
        end;
      end;
      if FMesh.MeshType = mt3D then
      begin
        OpenTempFile(TempFileName);
        try
          TempFileRoot := TempFileName + '_';
          TempFileName := ExtractFileName(TempFileRoot);
          for LayerIndex := 1 to FMesh.LayerCount do
          begin
            WriteString('@INSERT 99 ');
            WriteString(TempFileName + IntToStr(LayerIndex));
            Model.FilesToDelete.Add(TempFileName + IntToStr(LayerIndex));
            NewLine;

            OpenTempFile(TempFileRoot + IntToStr(LayerIndex));
            try
              InternalWrite15B(LayerIndex);
            finally
              CloseTempFile;
            end;
          end;
        finally
          CloseTempFile;
        end;
      end
      else
      begin
        OpenTempFile(TempFileName);
        try
          InternalWrite15B(0);
        finally
          CloseTempFile;
        end;
      end
    end
    else
    begin
      InternalWrite15B(-1);
    end;
  finally
    ElementList.Free;
  end;

  if UnsatRegion <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(UnsatRegion);
  end;
  Model.DataArrayManager.AddDataSetToCache(MaxPerm);
  if MidPerm <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(MidPerm);
  end;
  Model.DataArrayManager.AddDataSetToCache(MinPerm);
  Model.DataArrayManager.AddDataSetToCache(HorizAngle);
  if VerticalAngle <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(VerticalAngle);
  end;
  if RotationAngle <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(RotationAngle);
  end;

  Model.DataArrayManager.AddDataSetToCache(MaxLongDisp);
  if MidLongDisp <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(MidLongDisp);
  end;
  Model.DataArrayManager.AddDataSetToCache(MinLongDisp);

  Model.DataArrayManager.AddDataSetToCache(MaxTransvDisp);
  if MidTransvDisp <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(MidTransvDisp);
  end;
  if SIGMAS <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(SIGMAS);
  end;
  if SIGMAA <> nil then
  begin
    Model.DataArrayManager.AddDataSetToCache(SIGMAA);
  end;
  Model.DataArrayManager.AddDataSetToCache(MinTransvDisp);
end;

procedure TSutraInputWriter.WriteDataSet17;
var
  NodeIndex: Integer;
  Nodes: TArray<TPair<Integer, TBoundaryNode>>;
  ANode: TBoundaryNode;
begin
  Nodes := FFluidSourceNodes.ToArray;
  if Length(Nodes) > 0 then
  begin
    WriteCommentLine('Data set 17');
    for NodeIndex := 0 to Length(Nodes) - 1 do
    begin
      ANode := Nodes[NodeIndex].Value;
      WriteInteger(Abs(ANode.NodeNumber));
//      if ANode.UseBCTime then
//      begin
//        WriteInteger(-ANode.NodeNumber);
//      end
//      else
//      begin
//        WriteInteger(ANode.NodeNumber);
//      end;
      if WritingTemplate and (ANode.PressureOrFlowFormula <> '') then
      begin
        WriteString(ANode.PressureOrFlowFormula);
        FPestParamUsed := True;
      end
      else
      begin
        WriteFloat(ANode.PressureOrFlow);
        if ANode.PressureOrFlowFormula <> '' then
        begin
          FPestParamUsed := True;
        end;
      end;
      if WritingTemplate and (ANode.TempOrConcFormula <> '') then
      begin
        WriteString(ANode.TempOrConcFormula);
        FPestParamUsed := True;
      end
      else
      begin
        WriteFloat(ANode.TempOrConc);
        if ANode.TempOrConcFormula <> '' then
        begin
          FPestParamUsed := True;
        end;
      end;
//      WriteFloat(ANode.PressureOrFlow);
//      WriteFloat(ANode.TempOrConc);
      NewLine;
    end;
    WriteInteger(0);
    NewLine;
  end;

//  if FFluidSourceNodes.Count > 0 then
//  begin
//    WriteCommentLine('Data set 17');
//    for NodeIndex := 0 to FFluidSourceNodes.Count - 1 do
//    begin
//      WriteInteger(FFluidSourceNodes[NodeIndex]);
//      WriteFloat(0.0);
//      WriteFloat(0.0);
//      NewLine;
//    end;
//    WriteInteger(0);
//    NewLine;
//  end;
end;

procedure TSutraInputWriter.WriteDataSet18;
var
  NodeIndex: Integer;
  Nodes: TArray<TPair<Integer, TBoundaryNode>>;
  ANode: TBoundaryNode;
begin
  Nodes := FMassEnergySourceNodes.ToArray;
  if Length(Nodes) > 0 then
  begin
    WriteCommentLine('Data set 18');
    for NodeIndex := 0 to Length(Nodes) - 1 do
    begin
      ANode := Nodes[NodeIndex].Value;
      WriteInteger(Abs(ANode.NodeNumber));
//      if ANode.UseBCTime then
//      begin
//        WriteInteger(-ANode.NodeNumber);
//      end
//      else
//      begin
//        WriteInteger(ANode.NodeNumber);
//      end;
//      WriteFloat(ANode.PressureOrFlow);
      if WritingTemplate and (ANode.TempOrConcFormula <> '') then
      begin
        WriteString(ANode.TempOrConcFormula);
        FPestParamUsed := True;
      end
      else
      begin
        WriteFloat(ANode.TempOrConc);
        if ANode.TempOrConcFormula <> '' then
        begin
          FPestParamUsed := True;
        end;
      end;
//      WriteFloat(ANode.TempOrConc);
      NewLine;
    end;
    WriteInteger(0);
    NewLine;
  end;


//  if FMassEnergySourceNodes.Count > 0 then
//  begin
//    WriteCommentLine('Data set 18');
//    for NodeIndex := 0 to FMassEnergySourceNodes.Count - 1 do
//    begin
//      WriteInteger(FMassEnergySourceNodes[NodeIndex]);
//      WriteFloat(0.0);
//      NewLine;
//    end;
//    WriteInteger(0);
//    NewLine;
//  end;
end;

procedure TSutraInputWriter.WriteDataSet19;
var
  NodeIndex: Integer;
  Nodes: TArray<TPair<Integer, TBoundaryNode>>;
  ANode: TBoundaryNode;
begin
  Nodes := FSpecifiedPressureNodes.ToArray;
  if Length(Nodes) > 0 then
  begin
    WriteCommentLine('Data set 19');
    for NodeIndex := 0 to Length(Nodes) - 1 do
    begin
      ANode := Nodes[NodeIndex].Value;
      WriteInteger(Abs(ANode.NodeNumber));
//      if ANode.UseBCTime then
//      begin
//        WriteInteger(-ANode.NodeNumber);
//      end
//      else
//      begin
//        WriteInteger(ANode.NodeNumber);
//      end;
      if WritingTemplate and (ANode.PressureOrFlowFormula <> '') then
      begin
        WriteString(ANode.PressureOrFlowFormula);
        FPestParamUsed := True;
      end
      else
      begin
        WriteFloat(ANode.PressureOrFlow);
        if ANode.PressureOrFlowFormula <> '' then
        begin
          FPestParamUsed := True;
        end;
      end;
      if WritingTemplate and (ANode.TempOrConcFormula <> '') then
      begin
        WriteString(ANode.TempOrConcFormula);
        FPestParamUsed := True;
      end
      else
      begin
        WriteFloat(ANode.TempOrConc);
        if ANode.TempOrConcFormula <> '' then
        begin
          FPestParamUsed := True;
        end;
      end;
//      WriteFloat(ANode.PressureOrFlow);
//      WriteFloat(ANode.TempOrConc);
      NewLine;
    end;
    WriteInteger(0);
    NewLine;
  end;

//  if FSpecifiedPressureNodes.Count > 0 then
//  begin
//    WriteCommentLine('Data set 19');
//    for NodeIndex := 0 to FSpecifiedPressureNodes.Count - 1 do
//    begin
//      WriteInteger(FSpecifiedPressureNodes[NodeIndex]);
//      WriteFloat(0.0);
//      WriteFloat(0.0);
//      NewLine;
//    end;
//    WriteInteger(0);
//    NewLine;
//  end;
end;

procedure TSutraInputWriter.WriteDataSet9;
var
  COMPL: double;
  CL: double;
  RHOL0: Double;
  SIGMAL: Double;
  URHOL0: Double;
  DRLDU: Double;
  VISC0: Double;
  COMPI: Double;
  CI: Double;
  SIGMAI: Double;
  RHOI: Double;
begin
  WriteCommentLine('Data set 9');
  COMPL := FOptions.FluidCompressibility;

  CL := 0.;
  SIGMAL := 0.;
  URHOL0 := 0.;
  DRLDU := 0.;
  VISC0 := 0.;
  RHOL0 := 0.;
  COMPI := 0.;
  CI := 0.;
  SIGMAI := 0.;
  RHOI := 0.;
  case FOptions.TransportChoice of
    tcSolute:
      begin
        CL := 0.;
        SIGMAL := FOptions.FluidDiffusivity;
        URHOL0 := FOptions.BaseConcentration;
        DRLDU := FOptions.FluidDensityCoefficientConcentration;
        VISC0 := FOptions.Viscosity;
        RHOL0 := FOptions.BaseFluidDensity;
      end;
    tcSoluteHead:
      begin
        CL := 0.;
        SIGMAL := FOptions.FluidDiffusivity;
        URHOL0 := 0;
        DRLDU := 0;
        VISC0 := 1;
        RHOL0 := 1;
      end;
    tcEnergy:
      begin
        CL := FOptions.FluidSpecificHeat;
        SIGMAL := FOptions.FluidThermalConductivity;
        URHOL0 := FOptions.BaseTemperature;
        DRLDU := FOptions.FluidDensityCoefficientTemperature;
        VISC0 := FOptions.ScaleFactor;
        RHOL0 := FOptions.BaseFluidDensity;
      end;
    tcFreezing:
      begin
        CL := FOptions.FluidSpecificHeat;
        SIGMAL := FOptions.FluidThermalConductivity;
        URHOL0 := FOptions.BaseTemperature;
        DRLDU := FOptions.FluidDensityCoefficientTemperature;
        VISC0 := FOptions.ScaleFactor;
        RHOL0 := FOptions.BaseFluidDensity;
        COMPI := FOptions.IceCompressibility;
        CI := FOptions.IceSpecificHeat;
        SIGMAI := FOptions.IceThermalConductivity;
        RHOI := FOptions.IceDensity;
      end
    else
      Assert(False);
  end;
  WriteFloat(COMPL);
  WriteFloat(CL);
  WriteFloat(SIGMAL);
  WriteFloat(RHOL0);
  WriteFloat(URHOL0);
  WriteFloat(DRLDU);
  WriteFloat(VISC0);
  if FOptions.TransportChoice = tcFreezing then
  begin
    WriteFloat(COMPI);
    WriteFloat(CI);
    WriteFloat(SIGMAI);
    WriteFloat(RHOI);
  end;
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet20;
var
  NodeIndex: Integer;
  Nodes: TArray<TPair<Integer, TBoundaryNode>>;
  ANode: TBoundaryNode;
begin
  Nodes := FSpecifiedTempConcNodes.ToArray;
  if Length(Nodes) > 0 then
  begin
    WriteCommentLine('Data set 20');
    for NodeIndex := 0 to Length(Nodes) - 1 do
    begin
      ANode := Nodes[NodeIndex].Value;
      WriteInteger(Abs(ANode.NodeNumber));
//      if ANode.UseBCTime then
//      begin
//        WriteInteger(-ANode.NodeNumber);
//      end
//      else
//      begin
//        WriteInteger(ANode.NodeNumber);
//      end;
      if WritingTemplate and (ANode.TempOrConcFormula <> '') then
      begin
        WriteString(ANode.TempOrConcFormula);
        FPestParamUsed := True;
      end
      else
      begin
        WriteFloat(ANode.TempOrConc);
        if ANode.TempOrConcFormula <> '' then
        begin
          FPestParamUsed := True;
        end;
      end;
//      WriteFloat(ANode.TempOrConc);
      NewLine;
    end;
    WriteInteger(0);
    NewLine;
  end;
end;

procedure TSutraInputWriter.WriteDataSet21A;
var
  FlowNodes: IGeneralFlowNodes;
  NodeArray: TArray<TGeneralFlowNode>;
  NodeIndex: Integer;
  ANode: TGeneralFlowNode;
  ListIndex: Integer;
  NodeList: TList<IGeneralFlowNodes>;
  FoundFirst: Boolean;
begin
  if Model.ModelSelection = msSutra22 then
  begin
    Exit;
  end;
  FoundFirst := False;
  for ListIndex := 0 to FGeneralFlowNodes.Count - 1 do
  begin
    NodeList := FGeneralFlowNodes[ListIndex];
    if NodeList.Count > 0 then
    begin
      FlowNodes := NodeList[0];
      if FlowNodes.Count > 0 then
      begin
        if not FoundFirst then
        begin
          WriteCommentLine('Data set 21A');
          FoundFirst := True;
        end;
        NodeArray := FlowNodes.ToArray;
        for NodeIndex := 0 to Length(NodeArray) - 1 do
        begin
          ANode := NodeArray[NodeIndex];
          if ANode.FUseBCTime then
          begin
            WriteInteger(-ANode.NodeNumber-1);
          end
          else
          begin
            WriteInteger(ANode.NodeNumber+1);
          end;

          if WritingTemplate and (ANode.P1.Formula <> '') then
          begin
            WriteString(ANode.P1.Formula);
            FPestParamUsed := True;
          end
          else
          begin
            WriteFloat(ANode.P1.Value);
            if ANode.P1.Formula <> '' then
            begin
              FPestParamUsed := True;
            end;
          end;
          if WritingTemplate and (ANode.Q1.Formula <> '') then
          begin
            WriteString(ANode.Q1.Formula);
            FPestParamUsed := True;
          end
          else
          begin
            WriteFloat(ANode.Q1.Value);
            if ANode.Q1.Formula <> '' then
            begin
              FPestParamUsed := True;
            end;
          end;
          if WritingTemplate and (ANode.P2.Formula <> '') then
          begin
            WriteString(ANode.P2.Formula);
            FPestParamUsed := True;
          end
          else
          begin
            WriteFloat(ANode.P2.Value);
            if ANode.P2.Formula <> '' then
            begin
              FPestParamUsed := True;
            end;
          end;
          if WritingTemplate and (ANode.Q2.Formula <> '') then
          begin
            WriteString(ANode.Q2.Formula);
            FPestParamUsed := True;
          end
          else
          begin
            WriteFloat(ANode.Q2.Value);
            if ANode.Q2.Formula <> '' then
            begin
              FPestParamUsed := True;
            end;
          end;
          WriteLimit(ANode.Limit1);
          WriteLimit(ANode.Limit2);
          if WritingTemplate and (ANode.U1.Formula <> '') then
          begin
            WriteString(ANode.U1.Formula);
            FPestParamUsed := True;
          end
          else
          begin
            WriteFloat(ANode.U1.Value);
            if ANode.U1.Formula <> '' then
            begin
              FPestParamUsed := True;
            end;
          end;
          WriteExitSpec(ANode.ExitSpecification);
          if WritingTemplate and (ANode.U2.Formula <> '') then
          begin
            WriteString(ANode.U2.Formula);
            FPestParamUsed := True;
          end
          else
          begin
            WriteFloat(ANode.U2.Value);
            if ANode.U2.Formula <> '' then
            begin
              FPestParamUsed := True;
            end;
          end;

//          WriteFloat(ANode.P1.Value);
//          WriteFloat(ANode.Q1.Value);
//          WriteFloat(ANode.P2.Value);
//          WriteFloat(ANode.Q2.Value);
//          WriteLimit(ANode.Limit1);
//          WriteLimit(ANode.Limit2);
//          WriteFloat(ANode.U1.Value);
//          WriteExitSpec(ANode.ExitSpecification);
//          WriteFloat(ANode.U2.Value);
          NewLine;
        end;
      end
    end;
  end;
  if FoundFirst then
  begin
    WriteString('0');
    NewLine;
  end;
end;

procedure TSutraInputWriter.WriteDataSet21B;
var
  TransportNodes: IGeneralTransportNodes;
  NodeArray: TArray<TGeneralTransportNode>;
  NodeIndex: Integer;
  ANode: TGeneralTransportNode;
  FoundFirst: Boolean;
  ListIndex: Integer;
  TransList: TList<IGeneralTransportNodes>;
begin
  if Model.ModelSelection = msSutra22 then
  begin
    Exit;
  end;
  FoundFirst := False;
  for ListIndex := 0 to FGeneralTransportNodes.Count - 1 do
  begin
    TransList := FGeneralTransportNodes[ListIndex];
    if TransList.Count > 0 then
    begin
      TransportNodes := TransList[0];
      if TransportNodes.Count > 0 then
      begin
        if not FoundFirst then
        begin
          WriteCommentLine('Data set 21B');
          FoundFirst := True;
        end;
        NodeArray := TransportNodes.ToArray;
        for NodeIndex := 0 to Length(NodeArray) - 1 do
        begin
          ANode := NodeArray[NodeIndex];
          if ANode.FUseBCTime then
          begin
            WriteInteger(-ANode.NodeNumber-1);
          end
          else
          begin
            WriteInteger(ANode.NodeNumber+1);
          end;

          if WritingTemplate and (ANode.FUValue1.Formula <> '') then
          begin
            WriteString(ANode.FUValue1.Formula);
            FPestParamUsed := True;
          end
          else
          begin
            WriteFloat(ANode.FUValue1.Value);
          end;
          if WritingTemplate and (ANode.FSoluteEnergyInflow.Formula <> '') then
          begin
            WriteString(ANode.FSoluteEnergyInflow.Formula);
            FPestParamUsed := True;
          end
          else
          begin
            WriteFloat(ANode.FSoluteEnergyInflow.Value);
          end;
          if WritingTemplate and (ANode.FUValue2.Formula <> '') then
          begin
            WriteString(ANode.FUValue2.Formula);
            FPestParamUsed := True;
          end
          else
          begin
            WriteFloat(ANode.FUValue2.Value);
          end;
          if WritingTemplate and (ANode.FSoluteEnergyOutflow.Formula <> '') then
          begin
            WriteString(ANode.FSoluteEnergyOutflow.Formula);
            FPestParamUsed := True;
          end
          else
          begin
            WriteFloat(ANode.FSoluteEnergyOutflow.Value);
          end;

//          WriteFloat(ANode.FUValue1.Value);
//          WriteFloat(ANode.FSoluteEnergyInflow.Value);
//          WriteFloat(ANode.FUValue2.Value);
//          WriteFloat(ANode.FSoluteEnergyOutflow.Value);
          NewLine;
        end;
      end;
    end;
  end;
  if FoundFirst then
  begin
    WriteString('0');
    NewLine;
  end;

end;

procedure TSutraInputWriter.WriteDataSet22;
var
  ElementIndex: Integer;
  Element2D: TSutraElement2D;
  El2DList: TSutraElement2D_List;
  NodeIndex: Integer;
  Node2D: TSutraNode2D;
  El3DList: TSutraElement3DList;
  AnElement3D: TSutraElement3D;
  Node3D: TSutraNode3D;
begin
  WriteCommentLine('Data set 22');
  WriteString('INCIDENCE');
  NewLine;
  if FMesh.MeshType in [mt2D, mtProfile] then
  begin
    El2DList := TSutraElement2D_List.Create;
    try
      El2DList.Capacity := FMesh.Mesh2D.Elements.Count;
      for ElementIndex := 0 to FMesh.Mesh2D.Elements.Count - 1 do
      begin
        El2DList.Add(FMesh.Mesh2D.Elements[ElementIndex]);
      end;

      El2DList.Sort(TComparer<TSutraElement2D>.Construct(
        function (const L, R: TSutraElement2D): integer
        begin
          result := L.ElementNumber - R.ElementNumber;
        end));

      for ElementIndex := 0 to El2DList.Count - 1 do
      begin
        Element2D := El2DList[ElementIndex];
        Assert(Element2D.ElementNumber = ElementIndex);
        Assert(Element2D.Nodes.Count = 4);
        WriteInteger(Element2D.ElementNumber+1);
        for NodeIndex := 0 to Element2D.Nodes.Count - 1 do
        begin
          Node2D := Element2D.Nodes[NodeIndex].Node;
          WriteInteger(Node2D.Number+1);
        end;
        NewLine;
      end;
    finally
      El2DList.Free;
    end;
  end
  else
  begin
    El3DList := TSutraElement3DList.Create;
    try
      El3DList.Capacity := FMesh.ActiveElementCount;
      for ElementIndex := 0 to FMesh.Elements.Count - 1 do
      begin
        AnElement3D := FMesh.Elements[ElementIndex];
        if AnElement3D.Active then
        begin
          El3DList.Add(AnElement3D);
        end;
      end;

      El3DList.Sort(TComparer<TSutraElement3D>.Construct(
        function (const L, R: TSutraElement3D): integer
        begin
          result := L.ElementNumber - R.ElementNumber;
        end));

      for ElementIndex := 0 to El3DList.Count - 1 do
      begin
        AnElement3D := El3DList[ElementIndex];
        Assert(AnElement3D.ElementNumber = ElementIndex);
        Assert(AnElement3D.Nodes.Count = 8);
        WriteInteger(AnElement3D.ElementNumber+1);
        for NodeIndex := 4 to AnElement3D.Nodes.Count - 1 do
        begin
          Node3D := AnElement3D.Nodes[NodeIndex].Node;
          WriteInteger(Node3D.Number+1);
        end;
        for NodeIndex := 0 to 3 do
        begin
          Node3D := AnElement3D.Nodes[NodeIndex].Node;
          WriteInteger(Node3D.Number+1);
        end;
        NewLine;
      end;
    finally
      El3DList.Free;
    end;
  end;
end;

procedure TSutraInputWriter.WriteDataSet2A;
var
  SIMULA: AnsiString;
begin
  WriteCommentLine('Data set 2A');
  SIMULA := '''SUTRA VERSION ';
  case Model.ModelSelection of
    msSutra22: SIMULA := SIMULA + '2.2';
    msSutra30: SIMULA := SIMULA + '3.0';
    msSutra40: SIMULA := SIMULA + '4.0';
  else
    Assert(False);
  end;
  // If the model version is less than 4, use ENERGY TRANSPORT if
  // FREEZING TRANSPORT is selected.
  case FOptions.TransportChoice of
    tcSolute, tcSoluteHead:
      SIMULA := SIMULA + ' SOLUTE TRANSPORT''';
    tcEnergy:
      SIMULA := SIMULA + ' ENERGY TRANSPORT''';
    tcFreezing:
      begin
        Assert(Model.ModelSelection = msSutra40);
        SIMULA := SIMULA + ' FREEZING TRANSPORT''';
      end
  else
    Assert(False);
  end;
  WriteString(SIMULA);
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet2B;
var
  MSHSTR: string;
  LayerIndex: Integer;
  ColIndex: Integer;
  Node: TSutraNode3D;
  Layered: Boolean;
begin
  Layered := False;
  WriteCommentLine('Data set 2B');
  case Model.SutraMesh.MeshType of
    mt2D, mtProfile:
      begin
        MSHSTR := '''2D IRREGULAR''';
      end;
    mt3D:
      begin
        Layered := True;
        for LayerIndex := 0 to Model.LayerCount do
        begin
          for ColIndex := 0 to Model.SutraMesh.Mesh2D.Nodes.Count - 1 do
          begin
            Node := Model.SutraMesh.NodeArray[LayerIndex,ColIndex];
            if not Node.Active then
            begin
              Layered := False;
              break;
            end;
          end;
          if not Layered then
          begin
            break;
          end;
        end;
        if Layered then
        begin
          MSHSTR := '''3D LAYERED''';
        end
        else
        begin
          MSHSTR := '''3D IRREGULAR''';
          if HasLakes then
          begin
            frmErrorsAndWarnings.AddError(Model, StrLakesUsedWithIrre,
              StrSomeLayersInThis)
          end;
        end;
      end;
  else
    Assert(False);
  end;
  WriteString(MSHSTR);
  if Layered then
  begin
    WriteInteger(Model.LayerCount+1);
    WriteInteger(Model.SutraMesh.Mesh2D.Nodes.Count);
    WriteInteger(Model.SutraMesh.Mesh2D.Elements.Count);
    WriteString(' ACROSS');
  end;
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet3;
var
  NN: Integer;
  NE: Integer;
  NPBC: Integer;
  NUBC: Integer;
  NSOP: Integer;
  NSOU: Integer;
  NPBG: Integer;
  NUBG: Integer;
  ListIndex: Integer;
  AFlowList: TList<IGeneralFlowNodes>;
  ATransportList: TList<IGeneralTransportNodes>;
begin
  WriteCommentLine('Data set 3');
  NN := 0;
  NE := 0;
  case FMesh.MeshType of
    mt2D, mtProfile:
      begin
        NN := FMesh.Mesh2D.Nodes.Count;
        NE := FMesh.Mesh2D.Elements.Count;
      end;
    mt3D:
      begin
        NN := FMesh.ActiveNodeCount;
        NE := FMesh.ActiveElementCount;
      end;
  end;
  F_NN := NN;
  NPBC := FSpecifiedPressureNodes.Count;
  NUBC := FSpecifiedTempConcNodes.Count;
  NSOP := FFluidSourceNodes.Count;
  NSOU := FMassEnergySourceNodes.Count;

  NPBG := 0;
  NUBG := 0;
  if Model.ModelSelection <> msSutra22 then
  begin
    if Assigned(FGeneralFlowNodes) and (FGeneralFlowNodes.Count > 0)  then
    begin
      for ListIndex := 0 to FGeneralFlowNodes.Count - 1 do
      begin
        AFlowList := FGeneralFlowNodes[ListIndex];
        if AFlowList.Count > 0 then
        begin
          NPBG := NPBG + AFlowList[0].Count;
        end;
      end;
    end;

    if Assigned(FGeneralTransportNodes) and (FGeneralTransportNodes.Count > 0)  then
    begin
      for ListIndex := 0 to FGeneralTransportNodes.Count - 1 do
      begin
        ATransportList := FGeneralTransportNodes[ListIndex];
        if ATransportList.Count > 0 then
        begin
          NUBG := NUBG + ATransportList[0].Count;
        end;
      end;
    end;
  end;

  WriteInteger(NN);
  WriteInteger(NE);
  WriteInteger(NPBC);
  WriteInteger(NUBC);
  WriteInteger(NSOP);
  WriteInteger(NSOU);
  if Model.ModelSelection <> msSutra22 then
  begin
    WriteInteger(NPBG);
    WriteInteger(NUBG);
  end;
  WriteInteger(FNOBS);
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet4;
var
  CUNSAT: AnsiString;
  CSSFLO: AnsiString;
  CSSTRA: AnsiString;
  CREAD: AnsiString;
  ISTORE: integer;
begin
  WriteCommentLine('Data set 4');
  case FOptions.SaturationChoice of
    scSaturated: CUNSAT := '''SATURATED'' ';
    scUnsaturated: CUNSAT := '''UNSATURATED'' ';
    else Assert(False);
  end;

  case FOptions.SimulationType of
    stSteadyFlowSteadyTransport:
      begin
        CSSFLO := '''STEADY FLOW'' ';
        CSSTRA := '''STEADY TRANSPORT'' ';
      end;
    stSteadyFlowTransientTransport:
      begin
        CSSFLO := '''STEADY FLOW'' ';
        CSSTRA := '''TRANSIENT TRANSPORT'' ';
      end;
    stTransientFlowTransientTransport:
      begin
        CSSFLO := '''TRANSIENT FLOW'' ';
        CSSTRA := '''TRANSIENT TRANSPORT'' ';
      end;
    else
      Assert(False);
  end;

  case FOptions.StartType of
    stCold: CREAD := '''COLD'' ';
    stWarm: CREAD := '''WARM'' ';
  end;

  ISTORE := FOptions.RestartFrequency;

  WriteString(CUNSAT);
  WriteString(CSSFLO);
  WriteString(CSSTRA);
  WriteString(CREAD);
  WriteInteger(ISTORE);
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet5;
var
  UP: double;
  GNUP: double;
  GNUU: Double;
begin
  WriteCommentLine('Data set 5');
  UP := FOptions.FractionalUpstreamWeight;
  WriteFloat(UP);
  if Model.ModelSelection = msSutra22 then
  begin
    GNUP := FOptions.PressureFactor;
    GNUU := FOptions.UFactor;
    WriteFloat(GNUP);
    WriteFloat(GNUU);
  end;
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet6;
var
  Index: Integer;
begin
  WriteCommentLine('Data set 6');
    for Index := 0 to FSchedules.Count - 1 do
    begin
      WriteString(FSchedules[Index]);
      NewLine;
    end;
end;

procedure TSutraInputWriter.WriteDataSet7A;
var
  ITRMAX: Integer;
  RPMAX: Double;
  RUMAX: Double;
begin
  WriteCommentLine('Data set 7A');
  ITRMAX := FOptions.MaxIterations;
  WriteInteger(ITRMAX);

  if ITRMAX <> 1 then
  begin
    RPMAX := FOptions.NonLinPressureCriterion;
    RUMAX := FOptions.UCriterion;
    WriteFloat(RPMAX);
    WriteFloat(RUMAX);
  end;

  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet7B;
var
  CSOLVP: AnsiString;
  ITRMXP: Integer;
  TOLP: Double;
begin
  WriteCommentLine('Data set 7B');
  case FOptions.PresSolutionMethod of
    psmDirect:
    begin
      CSOLVP := '''DIRECT'' ';
      if F_NN > 1000 then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrDirectSolverUsed,
          StrTheDirectSolverPressure);
      end;
    end;
    pcmCG: CSOLVP := '''CG'' ';
    psmGMRES: CSOLVP := '''GMRES'' ';
    psmOthomin: CSOLVP := '''ORTHOMIN'' ';
  end;
  WriteString(CSOLVP);

  if FOptions.PresSolutionMethod <> psmDirect then
  begin
    ITRMXP := FOptions.MaxPressureIterations;
    TOLP := FOptions.PressureCriterion;
    WriteInteger(ITRMXP);
    WriteFloat(TOLP);
  end;
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet7C;
var
  CSOLVU: AnsiString;
  ITRMXU: Integer;
  TOLU: Double;
begin
  WriteCommentLine('Data set 7C');
  case FOptions.USolutionMethod of
    usmDirect:
      begin
        CSOLVU := '''DIRECT'' ';
        if F_NN > 1000 then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrDirectSolverUsed,
            StrTheDirectSolverConc);
        end;
      end;
    usmGMRES: CSOLVU := '''GMRES'' ';
    usmOthomin: CSOLVU := '''ORTHOMIN'' ';
  end;
  WriteString(CSOLVU);

  if FOptions.USolutionMethod <> usmDirect then
  begin
    ITRMXU := FOptions.MaxTransportIterations;
    TOLU := FOptions.TransportCriterion;
    WriteInteger(ITRMXU);
    WriteFloat(TOLU);
  end;
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet8A;
var
  NPRINT: integer;
  CNODAL: AnsiString;
  CELMNT: AnsiString;
  CINCID: AnsiString;
  CPANDS: AnsiString;
  CVEL: AnsiString;
  CCORT: AnsiString;
  CBUDG: AnsiString;
  CSCRN: AnsiString;
  CPAUSE: AnsiString;
begin
  WriteCommentLine('Data set 8A');
  NPRINT := FOutputControl.ListingPrintFrequency;
  if not (sloNPrint in FOutputControl.ListingOptions) then
  begin
    NPRINT := -NPRINT;
  end;

  if sloCNodal in FOutputControl.ListingOptions then
  begin
    CNODAL := ' ''Y'' ';
  end
  else
  begin
    CNODAL := ' ''N'' ';
  end;

  if sloCElment in FOutputControl.ListingOptions then
  begin
    CELMNT := ' ''Y'' ';
  end
  else
  begin
    CELMNT := ' ''N'' ';
  end;

  if sloCIncid in FOutputControl.ListingOptions then
  begin
    CINCID := ' ''Y'' ';
  end
  else
  begin
    CINCID := ' ''N'' ';
  end;

  if sloCPandS in FOutputControl.ListingOptions then
  begin
    CPANDS := ' ''Y'' ';
  end
  else
  begin
    CPANDS := ' ''N'' ';
  end;

  if sloCVel in FOutputControl.ListingOptions then
  begin
    CVEL := ' ''Y'' ';
  end
  else
  begin
    CVEL := ' ''N'' ';
  end;

  if sloCCorT in FOutputControl.ListingOptions then
  begin
    CCORT := ' ''Y'' ';
  end
  else
  begin
    CCORT := ' ''N'' ';
  end;

  if sloCBudg in FOutputControl.ListingOptions then
  begin
    CBUDG := ' ''Y'' ';
  end
  else
  begin
    CBUDG := ' ''N'' ';
  end;

  if sloCScrn in FOutputControl.ListingOptions then
  begin
    CSCRN := ' ''Y'' ';
  end
  else
  begin
    CSCRN := ' ''N'' ';
  end;

  if sloCPause in FOutputControl.ListingOptions then
  begin
    CPAUSE := ' ''Y'' ';
  end
  else
  begin
    CPAUSE := ' ''N'' ';
  end;
  WriteInteger(NPRINT);
  WriteString(CNODAL);
  WriteString(CELMNT);
  WriteString(CINCID);
  WriteString(CPANDS);
  WriteString(CVEL);
  WriteString(CCORT);
  WriteString(CBUDG);
  WriteString(CSCRN);
  WriteString(CPAUSE);
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet8B;
var
  NCOLPR: integer;
begin
  WriteCommentLine('Data set 8B');
  NCOLPR := FOutputControl.NE_PrintFrequency;
  if not (neoPrintFirst in FOutputControl.NodeElementOptions) then
  begin
    NCOLPR := -NCOLPR;
  end;
  WriteInteger(NCOLPR);
  if neoNumber in FOutputControl.NodeElementOptions then
  begin
    WriteString(' ''N'' ');
  end;
  if neoCoordinates in FOutputControl.NodeElementOptions then
  begin
    WriteString(' ''X'' ''Y'' ');
    if (Model.SutraMesh.MeshType = mt3D) then
    begin
      WriteString('''Z'' ');
    end;
  end;
  if neoPressure in FOutputControl.NodeElementOptions then
  begin
    WriteString(' ''P'' ');
  end;
  if neoU in FOutputControl.NodeElementOptions then
  begin
    WriteString(' ''U'' ');
  end;
  if Model.ModelSelection = msSutra40 then
  begin
    if neoLiquidSaturation in FOutputControl.NodeElementOptions then
    begin
      WriteString(' ''L'' ');
    end;
    if neoIceSaturation in FOutputControl.NodeElementOptions then
    begin
      WriteString(' ''I'' ');
    end;
  end;
  if neoSaturation in FOutputControl.NodeElementOptions then
  begin
    WriteString(' ''S'' ');
  end;
  WriteString(' ''-''');
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet8C;
var
  LCOLPR: integer;
begin
  WriteCommentLine('Data set 8C');
  LCOLPR := FOutputControl.NE_PrintFrequency;
//  if not (neoPrintFirst in FOutputControl.NodeElementOptions) then
//  begin
//    LCOLPR := -LCOLPR;
//  end;
  WriteInteger(LCOLPR);
  if (neoNumber in FOutputControl.NodeElementOptions) then
  begin
    WriteString(' ''E'' ');
  end;
  if neoCoordinates in FOutputControl.NodeElementOptions then
  begin
    WriteString(' ''X'' ''Y'' ');
    if (Model.SutraMesh.MeshType = mt3D) then
    begin
      WriteString('''Z'' ');
    end;
  end;
  if neoVelocities in FOutputControl.NodeElementOptions then
  begin
    WriteString(' ''VX'' ''VY'' ');
    if (Model.SutraMesh.MeshType = mt3D) then
    begin
      WriteString('''VZ'' ');
    end;
  end;
  if frmGoPhast.ModelSelection = msSutra40 then
  begin
    if neoDarcyVelocities in FOutputControl.NodeElementOptions then
    begin
      WriteString(' ''qX'' ''qY'' ');
      if (Model.SutraMesh.MeshType = mt3D) then
      begin
        WriteString('''qZ'' ');
      end;
    end;
  end;
  WriteString(' ''-''');
  NewLine;
end;

procedure TSutraInputWriter.WriteDataSet8D;
var
//  DS8D_FileName: string;
//  StringList: TStringList;
  Index: Integer;
begin
  if FNOBS > 0 then
  begin
    WriteCommentLine('Data set 8D');
//    DS8D_FileName := ChangeFileExt(FFileName, '.8d');
//    Assert(FileExists(DS8D_FileName));
//    StringList := TStringList.Create;
//    try
//      StringList.LoadFromFile(DS8D_FileName);
      for Index := 0 to FObservations.Count - 1 do
      begin
        WriteString(FObservations[Index]);
        NewLine;
      end;
//    finally
//      StringList.Free;
//    end;

//    DS8D_FileName := ''''+ DS8D_FileName + '''';
//    WriteString('@INSERT 53 ');
//    WriteString(DS8D_FileName);
//    NewLine;
  end;
end;

procedure TSutraInputWriter.WriteDataSet8E;
var
  NBCFPR: Integer;
  NBCSPR: Integer;
  NBCPPR: Integer;
  NBCUPR: Integer;
  NBGPPR: Integer;
  NBGUPR: Integer;
  CINACT: AnsiString;
begin
  WriteCommentLine('Data set 8E');
  NBCFPR := FOutputControl.FluidSourcePrintFrequency;
  NBCSPR := FOutputControl.SoluteEnergySourcePrintFrequency;
  NBCPPR := FOutputControl.SpecifiedPressurePrintFrequency;
  NBCUPR := FOutputControl.SpecifiedConcTempPrintFrequency;
  NBGPPR := FOutputControl.GeneralizedFlowPrintFrequency;
  NBGUPR := FOutputControl.GeneralizedTransportPrintFrequency;
  if FOutputControl.ListAll then
  begin
    CINACT := ' ''Y''';
  end
  else
  begin
    CINACT := ' ''N''';
  end;
  WriteInteger(NBCFPR);
  WriteInteger(NBCSPR);
  WriteInteger(NBCPPR);
  WriteInteger(NBCUPR);
  if Model.ModelSelection <> msSutra22 then
  begin
    WriteInteger(NBGPPR);
    WriteInteger(NBGUPR);
  end;
  WriteString(CINACT);
  NewLine;
end;

procedure TSutraInputWriter.WriteFile(FileName: string; FluidSourceNodes,
  MassEnergySourceNodes, SpecifiedPressureNodes,
  SpecifiedTempConcNodes: IBoundaryNodes; NOBS: integer;
  Schedules, Observations: TStringList;
  GeneralFlowNodes: TObjectList<TList<IGeneralFlowNodes>>;
  GeneralTransportNodes: TObjectList<TList<IGeneralTransportNodes>>);
begin

  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMaxPermMinPerm);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMaxKMinK);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMaxPermMidPerm);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMaxKMidK);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMidPermMinPerm);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrMidKMinK);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDirectSolverUsed);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrLakesUsedWithIrre);
//    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDispersivityMayBe);


    FFileName := ChangeFileExt(FileName, '.inp');
    FSchedules := Schedules;
    FObservations := Observations;

    FFluidSourceNodes := FluidSourceNodes;
    FMassEnergySourceNodes := MassEnergySourceNodes;
    FSpecifiedPressureNodes := SpecifiedPressureNodes;
    FSpecifiedTempConcNodes := SpecifiedTempConcNodes;
    FGeneralFlowNodes := GeneralFlowNodes;
    FGeneralTransportNodes := GeneralTransportNodes;
    FNOBS := NOBS;

    FNameOfFile := FFileName;
    WriteFileInternal;

    SutraFileWriter.AddFile(sftInp, FFileName);

    if  Model.PestUsed and FPestParamUsed then
    begin
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteFileInternal;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TSutraInputWriter.WriteFileInternal;
begin
  //    FInputFileName := FFileName;
  OpenFile(FNameOfFile);
  try
    WriteTemplateHeader;
    WriteDataSet0;
    WriteDataSet1;
    WriteDataSet2A;
    WriteDataSet2B;
    WriteDataSet3;
    WriteDataSet4;
    WriteDataSet5;
    WriteDataSet6;
    WriteDataSet7A;
    WriteDataSet7B;
    WriteDataSet7C;
    WriteDataSet8A;
    WriteDataSet8B;
    WriteDataSet8C;
    WriteDataSet8D;
    WriteDataSet8E;
    WriteDataSet9;
    WriteDataSet10;
    WriteDataSet11;
    WriteDataSet12;
    WriteDataSet13;
    WriteDataSet14A;
    WriteDataSet14B;
    WriteDataSet15A;
    WriteDataSet15B;
    WriteDataSet17;
    WriteDataSet18;
    WriteDataSet19;
    WriteDataSet20;
    WriteDataSet21A;
    WriteDataSet21B;
    WriteDataSet22;
  finally
    CloseFile;
  end;
end;

function TSutraInputWriter.EvaluateFormula(Formula: string;
  const DataIdentifier: string; out PestParamName: string): double;
var
  Param: TModflowSteadyParameter;
  Compiler: TRbwParser;
  ErrorFunction: string;
  Expression: TExpression;
  ResultTypeOK: Boolean;
  ErrorMessage: string;
  procedure HandleError(E: Exception);
  var
    ErrorMessage: string;
    Formula: string;
  begin
    ErrorMessage := Format(StrErrorInTheFormula, [DataIdentifier, E.message]);
    frmFormulaErrors.AddFormulaError('', DataIdentifier,
      Expression.Decompile, ErrorMessage);
    Formula := '0';
    Compiler.Compile(Formula);
    Expression := Compiler.CurrentExpression;
    Expression.Evaluate;
  end;
begin
  Param := Model.GetPestParameterByName(Formula);
  if Param <> nil then
  begin
    Param.IsUsedInTemplate := True;
    result := Param.Value;
    PestParamName := Param.ParameterName;
    Exit;
  end
  else
  begin
    PestParamName := '';
  end;


  Compiler := Model.rpThreeDFormulaCompiler;
  ErrorFunction := Formula;
  try
    Compiler.Compile(Formula);
  except on E: ERbwParserError do
    begin
      ErrorMessage := Format(StrErrorInTheFormula, [DataIdentifier, E.message]);
      frmFormulaErrors.AddFormulaError('',
        DataIdentifier, Formula, ErrorMessage);
      Formula := '0';
      Compiler.Compile(Formula);
    end;
  end;

  Expression := Compiler.CurrentExpression;
  ResultTypeOK := Expression.ResultType in [rdtInteger, rdtDouble];
  if not ResultTypeOK then
  begin
    raise EInvalidDataType.Create(StrInvalidDataType, Formula);
  end;

  try
    Expression.Evaluate;
  except
    on E: ERbwParserError do
    begin
      HandleError(E);
    end;
    on E: EInvalidOp do
    begin
      HandleError(E);
    end;
    on E: EDivByZero do
    begin
      HandleError(E);
    end;
    on E: EZeroDivide do
    begin
      HandleError(E);
    end;
  end;

  result := Expression.DoubleResult;

end;

class function TSutraInputWriter.Extension: string;
begin
  Assert(False);
end;

procedure TSutraInputWriter.SetHasLakes(const Value: Boolean);
begin
  FHasLakes := Value;
end;

procedure TSutraInputWriter.WriteDataSet0;
begin
  WriteCommentLine(File_Comment('Main SUTRA input file'));
end;

end.
