program GW_Chart;



uses
  FastMM4 in 'FastMM4.pas',
  Forms,
  frmModChartUnit in 'frmModChartUnit.pas' {frmModChart},
  frmAboutUnit in 'frmAboutUnit.pas' {frmAbout},
  frmFormatUnit in 'frmFormatUnit.pas' {frmFormat},
  IntListUnit in 'IntListUnit.pas',
  ReaderUnit in 'ReaderUnit.pas' {frmZoneBdgtReader},
  frmLakePlotUnit in 'frmLakePlotUnit.pas' {frmLakePlot},
  ExtractUnit in 'ExtractUnit.pas' {frmExtract},
  PiperGraphUnit in 'PiperGraphUnit.pas' {frmPiperGraph},
  PiperFormatUnit in 'PiperFormatUnit.pas' {frmPiperFormat},
  frmEditCO3Unit in 'frmEditCO3Unit.pas' {frmEditCO3},
  MyFormUnit in 'MyFormUnit.pas' {MyForm},
  frmFlowReader in 'frmFlowReader.pas' {frmCellFlows},
  ChartRangeUnit in 'ChartRangeUnit.pas',
  frmSelectDiscretizationUnit in 'frmSelectDiscretizationUnit.pas' {frmSelectDiscretization},
  framFilePathUnit in 'framFilePathUnit.pas' {framFilePath: TFrame},
  RealListUnit in 'RealListUnit.pas',
  ReadModflowArrayUnit in 'ReadModflowArrayUnit.pas',
  frmConvertFlowsUnit in 'frmConvertFlowsUnit.pas' {frmConvertFlows},
  ObjectStringList in 'ObjectStringList.pas',
  frmBudgetPrecisionQueryUnit in 'frmBudgetPrecisionQueryUnit.pas' {frmBudgetPrecisionQuery},
  frmFarmUnit in 'frmFarmUnit.pas' {frmFarm},
  frmFarmFileTypeUnit in 'frmFarmFileTypeUnit.pas' {frmFarmFileType},
  DisclaimerTextUnit in 'DisclaimerTextUnit.pas',
  Mf6ObsUtilOutputReaderUnit in 'Mf6ObsUtilOutputReaderUnit.pas',
  frmModflowModelUnitsUnit in 'frmModflowModelUnitsUnit.pas' {frmModflowModelUnits};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmModChart, frmModChart);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmFormat, frmFormat);
  Application.CreateForm(TfrmZoneBdgtReader, frmZoneBdgtReader);
  Application.CreateForm(TfrmLakePlot, frmLakePlot);
  Application.CreateForm(TfrmExtract, frmExtract);
  Application.CreateForm(TfrmPiperGraph, frmPiperGraph);
  Application.CreateForm(TfrmPiperFormat, frmPiperFormat);
  Application.CreateForm(TfrmEditCO3, frmEditCO3);
  Application.CreateForm(TfrmCellFlows, frmCellFlows);
  Application.CreateForm(TfrmConvertFlows, frmConvertFlows);
  Application.CreateForm(TfrmBudgetPrecisionQuery, frmBudgetPrecisionQuery);
  Application.CreateForm(TfrmFarm, frmFarm);
  Application.CreateForm(TfrmFarmFileType, frmFarmFileType);
  Application.CreateForm(TfrmModflowModelUnits, frmModflowModelUnits);
  HandleParams;
  Application.Run;
end.
