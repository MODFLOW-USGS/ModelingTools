unit frmFmpFormulaEditorUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.ExtCtrls,
  RbwParser, Vcl.StdCtrls, JvExStdCtrls, JvRichEdit, Vcl.ComCtrls, JvExExtCtrls,
  JvNetscapeSplitter, Vcl.Buttons, frmFormulaUnit, Winapi.RichEdit,
  System.Contnrs;

type
  TfrmFmpFormulaEditor = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    btnFunctionHelp: TBitBtn;
    pnlMain: TPanel;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    pnlButtons: TPanel;
    gbLogicalOperators: TGroupBox;
    btnNotEqual: TButton;
    btnEquals: TButton;
    btnGreaterThan: TButton;
    btnLessThan: TButton;
    btnLessEquals: TButton;
    btnGreaterOrEquals: TButton;
    gbNumbers: TGroupBox;
    btn7: TButton;
    btn8: TButton;
    btn9: TButton;
    btn6: TButton;
    btn5: TButton;
    btn4: TButton;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn0: TButton;
    btnE: TButton;
    btnDecimal: TButton;
    gbOperators: TGroupBox;
    btnOpenParen: TButton;
    btnCloseParen: TButton;
    btnDivide: TButton;
    btnMultiply: TButton;
    btnPlus: TButton;
    btnMinus: TButton;
    btnComma: TButton;
    btnPower1: TButton;
    tvFormulaDiagram: TTreeView;
    jreFormula: TJvRichEdit;
    pnlRight: TPanel;
    pnlLabelItemTree: TPanel;
    lbltems: TLabel;
    tvItems: TTreeView;
    rbFormulaParser: TRbwParser;
    Splitter: TSplitter;
    Timer: TTimer;
    btnOpenBracket: TButton;
    btnCloseBracket: TButton;
    btnAndOperator: TButton;
    btnOrOperator: TButton;
    procedure FormDestroy(Sender: TObject); override;
    procedure FormCreate(Sender: TObject); override;
    procedure btnClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure jreFormulaChange(Sender: TObject);
    procedure jreFormulaDblClick(Sender: TObject);
    procedure jreFormulaMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure jreFormulaSelectionChange(Sender: TObject);
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept:
        Boolean);
    procedure TimerSetSelection(Sender: TObject);
    procedure tvFormulaDiagramCollapsed(Sender: TObject; Node: TTreeNode);
    procedure tvFormulaDiagramExpanded(Sender: TObject; Node: TTreeNode);
    procedure tvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure tvItemsDblClick(Sender: TObject);
    procedure tvItemsMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
  private
    FResultSet: boolean;
    FFunctions: TTreeNode;
    FDiagramObjectStorage: TList;
    FClickSelectionStart: Integer;
    FNewSelectionStart: Integer;
    FNewSelectionLength: Integer;
    FUpdating: Boolean;
    FSetColor: Boolean;
    FLastButton: TButton;
    FSelectedNode: TTreeNode;
    FFunctionHelpString: string;
    function GetFormula: string;
    procedure InsertText(const NewText: string);
    procedure SetFormula(const Value: string);
    procedure DiagramFormula;
    procedure MatchEndingParen(PriorSelection: TCharRange);
    procedure MatchStartingParen(PriorSelection: TCharRange);
    procedure CreateNodesForVariables;
    procedure CreatePredefinedVariables;
    procedure RemoveUnsupportedFunctions;
    procedure RemoveUnsupportedOrChangedOperators;
    procedure DefineAndOperator;
    procedure DefinOrOperator;
    procedure DefineEqualsOperator;
    procedure DefineNotEqualsOperator;
    procedure DefineGreaterThanOperator;
    procedure DefineLessThanOperator;
    procedure DefineGreaterThanOrEqualsOperator;
    procedure DefineLessThanOrEqualsOperator;
    procedure DefineNewOperators;
    procedure AddNewFunctions;
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure Initialize;
    property Formula: string read GetFormula write SetFormula;
    property ResultSet: boolean read FResultSet;
    procedure UpdateTreeList;
    { Public declarations }
  end;

var
  frmFmpFormulaEditor: TfrmFmpFormulaEditor;

implementation

uses
  System.StrUtils, IntListUnit, ClassificationUnit, System.Math;

resourcestring
  StrErrorAppendingRTF = 'Error appending RTF data.';
  StrDataSets = 'Data Sets';
  StrFunctions = 'Functions';
  StrYouCanCheckTheDo = 'You can check the documentation for any functions y' +
  'ou are using in the ModelMuse help.';
  StrFarmProcess = 'Farm Process';
{$R *.dfm}

var
  PriorSelectedText: string = '';

var
  AndOperator: TFunctionClass;
  OrOperator: TFunctionClass;
  EqualRROperator: TFunctionClass;
  EqualRIOperator: TFunctionClass;
  EqualIROperator: TFunctionClass;
  EqualIOperator: TFunctionClass;
  EqualBOperator: TFunctionClass;
  NotEqualRROperator: TFunctionClass;
  NotEqualRIOperator: TFunctionClass;
  NotEqualIROperator: TFunctionClass;
  NotEqualIOperator: TFunctionClass;
  NotEqualBOperator: TFunctionClass;

  LessThanIOperator: TFunctionClass;
  LessThanRROperator: TFunctionClass;
  LessThanRIOperator: TFunctionClass;
  LessThanIROperator: TFunctionClass;

  GreaterThanIOperator: TFunctionClass;
  GreaterThanRROperator: TFunctionClass;
  GreaterThanRIOperator: TFunctionClass;
  GreaterThanIROperator: TFunctionClass;

  LessThanOrEqualsIOperator: TFunctionClass;
  LessThanOrEqualsRROperator: TFunctionClass;
  LessThanOrEqualsRIOperator: TFunctionClass;
  LessThanOrEqualsIROperator: TFunctionClass;

  GreaterThanOrEqualsIOperator: TFunctionClass;
  GreaterThanOrEqualsRROperator: TFunctionClass;
  GreaterThanOrEqualsRIOperator: TFunctionClass;
  GreaterThanOrEqualsIROperator: TFunctionClass;

  CEILING_Function: TFunctionRecord;
  FLOOR_Function: TFunctionRecord;
  LOG_Function: TFunctionRecord;
  NEG_Function: TFunctionRecord;
  NEG_TO_ZERO_Function: TFunctionRecord;
  POS_TO_ZERO_Function: TFunctionRecord;
  TRUNCATE_Function: TFunctionRecord;

procedure TfrmFmpFormulaEditor.FormDestroy(Sender: TObject);
begin
  FDiagramObjectStorage.Free;
  inherited;
end;

procedure TfrmFmpFormulaEditor.FormCreate(Sender: TObject);
begin
  inherited;
  jreFormula.DoubleBuffered := False;
  Constraints.MinWidth := Width;
  pnlMain.Constraints.MinWidth := pnlMain.Width;
  pnlButtons.Constraints.MinWidth := pnlButtons.Width;

  FDiagramObjectStorage := TObjectList.Create;
end;

procedure TfrmFmpFormulaEditor.btnClick(Sender: TObject);
var
  //  AString: string;
  NewText: string;
  //  Start, sLength: integer;
  UseSpaces: boolean;
  Index: integer;
  //  Sel: TMemoSelection;
  SomeButtons: array[0..17] of TButton;
  ButtonText: string;
begin
  SomeButtons[0] := btn1;
  SomeButtons[1] := btn2;
  SomeButtons[2] := btn3;
  SomeButtons[3] := btn4;
  SomeButtons[4] := btn5;
  SomeButtons[5] := btn6;
  SomeButtons[6] := btn7;
  SomeButtons[7] := btn8;
  SomeButtons[8] := btn9;
  SomeButtons[9] := btn0;
  SomeButtons[10] := btnDecimal;
  SomeButtons[11] := btnOpenParen;
  SomeButtons[12] := btnCloseParen;
  SomeButtons[13] := btnComma;
  SomeButtons[15] := btnE;
  SomeButtons[16] := btnOpenBracket;
  SomeButtons[17] := btnCloseBracket;
  UseSpaces := True;
  for Index := Low(SomeButtons) to High(SomeButtons) do
  begin
    if Sender = SomeButtons[Index] then
    begin
      UseSpaces := False;
      break;
    end;
  end;

  if (FLastButton = btnE) and ((Sender = btnPlus) or (Sender = btnMinus)) then
  begin
    UseSpaces := False;
  end;

  if Sender = btnAndOperator then
  begin
    ButtonText := '&';
  end
  else
  begin
    ButtonText := (Sender as TButton).Caption;
  end;

  if UseSpaces then
  begin
    NewText := ' ' + ButtonText + ' ';
  end
  else
  begin
    NewText := ButtonText;
  end;

  InsertText(NewText);
  FLastButton := Sender as TButton;
end;

{ TfrmFmpFormulaEditor }

procedure TfrmFmpFormulaEditor.CreateNodesForVariables;
var
  Classifications: TStringList;
  AVariable: TCustomValue;
  Index: Integer;
  VariableList: TClassificationList;
  VarEdit: TVariableEdit;
  Position: integer;
  Node: TTreeNode;
  DuplicateVarCheck: TList;
begin
  { TODO : Nearly the same code is use in TfrmFormulaUnit, TFrmGridColor,
  TfrmScreenObjectProperties, and TfrmDataSets. Find a way to combine them. }
  if rbFormulaParser.VariableCount > 0 then
  begin
    VariableList := TClassificationList.Create;
    try
      DuplicateVarCheck := TList.Create;
      try
        for Index := 0 to rbFormulaParser.VariableCount - 1 do
        begin
          AVariable := rbFormulaParser.Variables[Index];
          if DuplicateVarCheck.IndexOf(AVariable) >= 0 then
          begin
            Continue;
          end;
          DuplicateVarCheck.Add(AVariable);
          VarEdit := TVariableEdit.Create(AVariable);
          VariableList.Add(VarEdit);
        end;
      finally
        DuplicateVarCheck.Free;
      end;
      Classifications := TStringList.Create;
      try
        ClassifyListedObjects(Classifications, VariableList, []);

        CreateClassifiedNodes(Classifications, 0, tvItems, '');

        // In each node, replace the TVariableEdit with the
        // TCustomValue that it holds.
        for Index := 0 to tvItems.Items.Count - 1 do
        begin
          Node := tvItems.Items[Index];
          VarEdit := Node.Data;
          if VarEdit <> nil then
          begin
            Node.Data := VarEdit.Variable;
          end;
        end;
      finally
        Classifications.Free;
      end;
    finally
      for Index := 0 to VariableList.Count - 1 do
      begin
        VariableList[Index].Free;
      end;
      VariableList.Free;
    end;
  end;
end;

procedure TfrmFmpFormulaEditor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := 0;
end;

procedure TfrmFmpFormulaEditor.CreatePredefinedVariables;
begin
  rbFormulaParser.CreateVariable('LR', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('ECw', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('ECe', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('CU', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('ETr', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('ETc', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('ETp', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('ETi', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('CIR', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('DMD', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('P', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('AREA', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('Tgw', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('Tp', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('Ti', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('EFF', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('ROOT', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('CapF', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('DP_p', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('DP_i', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('DP', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('ADRS', StrFarmProcess, 0.0, '');
  rbFormulaParser.CreateVariable('ADMD', StrFarmProcess, 0.0, '');
end;

procedure TfrmFmpFormulaEditor.DiagramFormula;
const
  TAB_CHAR = #9;
var
  AFormula: string;
  DiagramList: TStringList;
  Index: Integer;
  TabCount: integer;
  NodeStack: TList;
  CharIndex: integer;
  Line: string;
  FunctionName: string;
  FunctionFormula: string;
  TabPosition: integer;
  Node: TTreeNode;
  NodeData: TTreeNodeTextStorage;
begin
  tvFormulaDiagram.Items.Clear;
  FDiagramObjectStorage.Clear;
  AFormula := Formula;
  if AFormula = '' then
  begin
    Exit;
  end;
  try
    if rbFormulaParser.Compile(AFormula) >= 0 then
    begin
      DiagramList := TStringList.Create;
      try
        rbFormulaParser.CurrentExpression.Diagram(DiagramList);
        NodeStack := TList.Create;
        try
          for Index := 0 to DiagramList.Count - 1 do
          begin
            Line := DiagramList[Index];
            TabCount := 0;
            for CharIndex := 1 to Length(Line) do
            begin
              if Line[CharIndex] = TAB_CHAR then
              begin
                Inc(TabCount)
              end
              else
              begin
                break;
              end;
            end;
            Line := Copy(Line, TabCount+1, MaxInt);
            TabPosition := Pos(TAB_CHAR, Line);
            FunctionName := Copy(Line, 1, TabPosition-1);
            FunctionFormula := Copy(Line, TabPosition+1, MAXINT);
            if TabCount >= NodeStack.Count then
            begin
              if NodeStack.Count > 0 then
              begin
                Node := NodeStack[NodeStack.Count-1];
              end
              else
              begin
                Node := nil;
              end;
              Node := tvFormulaDiagram.Items.AddChild(Node, FunctionFormula);
              NodeStack.Add(Node);
            end
            else
            begin
              Node := NodeStack[TabCount];
              Node := tvFormulaDiagram.Items.Add(Node, FunctionFormula);
              NodeStack[TabCount] := Node;
              NodeStack.Count := TabCount+1;
            end;
            NodeData := TTreeNodeTextStorage.Create;
            FDiagramObjectStorage.Add(NodeData);
            NodeData.OpenText := FunctionName;
            NodeData.ClosedText := FunctionFormula;
            Node.Data := NodeData;
          end;
        finally
          NodeStack.Free;
        end;
      finally
        DiagramList.Free;
      end;
    end;
  except on E: ErbwParserError do
    begin
      // ignore
    end;
  end;
end;

function TfrmFmpFormulaEditor.GetFormula: string;
begin
  result := jreFormula.Lines.Text;
  result := StringReplace(result, sLineBreak, '', [rfReplaceAll, rfIgnoreCase]);
  result := Trim(Result);
end;

procedure TfrmFmpFormulaEditor.DefineAndOperator;
var
  OperatorDefinition: TOperatorDefinition;
  ArgumentDef: TOperatorArgumentDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '&';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p1;
  OperatorDefinition.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := AndOperator;
  ArgumentDef.OperatorClass := TOperator;

  rbFormulaParser.AddOperator(OperatorDefinition);
end;

procedure TfrmFmpFormulaEditor.DefinOrOperator;
var
  OperatorDefinition: TOperatorDefinition;
  ArgumentDef: TOperatorArgumentDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '|';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p1;
  OperatorDefinition.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := OrOperator;
  ArgumentDef.OperatorClass := TOperator;

  rbFormulaParser.AddOperator(OperatorDefinition);
end;

procedure TfrmFmpFormulaEditor.DefineEqualsOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '==';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p1;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := EqualRROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := EqualIROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := EqualRIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := EqualIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := EqualBOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  rbFormulaParser.AddOperator(OperatorDefinition);
end;

procedure TfrmFmpFormulaEditor.DefineNotEqualsOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '!=';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p1;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := NotEqualRROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := NotEqualIROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := NotEqualRIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := NotEqualIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := NotEqualBOperator;
  ArgumentDef.OperatorClass := TOperator;

  rbFormulaParser.AddOperator(OperatorDefinition);
end;

procedure TfrmFmpFormulaEditor.DefineGreaterThanOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '>';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p1;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanRROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanRIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanIROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanIOperator;
  ArgumentDef.OperatorClass := TOperator;

  rbFormulaParser.AddOperator(OperatorDefinition);
end;

procedure TfrmFmpFormulaEditor.DefineLessThanOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '<';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p1;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanRROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanIROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanRIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanIOperator;
  ArgumentDef.OperatorClass := TOperator;

  rbFormulaParser.AddOperator(OperatorDefinition);
end;

procedure TfrmFmpFormulaEditor.DefineLessThanOrEqualsOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '<=';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p1;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanOrEqualsRROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanOrEqualsIROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanOrEqualsRIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanOrEqualsIOperator;
  ArgumentDef.OperatorClass := TOperator;

  rbFormulaParser.AddOperator(OperatorDefinition);
end;

procedure TfrmFmpFormulaEditor.DefineGreaterThanOrEqualsOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '>=';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p1;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanOrEqualsRROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanOrEqualsIROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanOrEqualsRIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanOrEqualsIOperator;
  ArgumentDef.OperatorClass := TOperator;

  rbFormulaParser.AddOperator(OperatorDefinition);
end;

procedure TfrmFmpFormulaEditor.Initialize;
begin
  rbFormulaParser.ClearExpressions;
  rbFormulaParser.SpecialImplementorList.Clear;
  rbFormulaParser.ClearVariables;

  jreFormula.Lines.Clear;
  tvItems.Items.Clear;
  tvFormulaDiagram.Items.Clear;

  RemoveUnsupportedOrChangedOperators;
  RemoveUnsupportedFunctions;
  DefineNewOperators;
  AddNewFunctions;
  CreatePredefinedVariables;
end;

procedure TfrmFmpFormulaEditor.InsertText(const NewText: string);
//const
//{$IFDEF LINUX}
////  BreakChar = [#10];
//  LineBreak = #10;
//{$ELSE}
////  BreakChar = [#10,#13];
//  LineBreak = #13#10;
//{$ENDIF}
var
  Start, sLength: integer;
  Index: integer;
  Position: integer;
//  NewExpression: string;
  TextToSelect: string;
//  NewStart: integer;
//  FormulaText: string;
//  SelLength: integer;
//  LineBreakCount: Integer;
//  PosStart: Integer;
//  LineBreakPos: Integer;
//  PriorStart: Integer;
begin
  try
    Start := jreFormula.SelStart;
//    SelLength := jreFormula.SelLength;
    jreFormula.SelText := NewText;
//    SelLength := jreFormula.SelLength;
//    if SelLength > 0 then
//    begin
//      FormulaText := jreFormula.Lines.Text;
//      Start := jreFormula.SelStart + 1;
//
//      LineBreakCount := 0;
//      PosStart := 1;
//      LineBreakPos :=PosEx(LineBreak, FormulaText, PosStart);
//      While (LineBreakPos < Start + LineBreakCount*1) do
//      begin
//        Inc(LineBreakCount);
//        PosStart := LineBreakPos+1;
//        LineBreakPos :=PosEx(LineBreak, FormulaText, PosStart);
//      end;
//
//      PriorStart := Start;
//      Start := Start + LineBreakCount*1;
//
//      Delete(FormulaText, Start, SelLength);
//      Insert(NewText, FormulaText, Start);
//      jreFormula.Text := FormulaText;
//      // eliminate line breaks.
//      Formula := Formula;
//
//      Start := PriorStart;
//
      Position := Pos('(', NewText);
      if (Position >= 1) and (NewText <> '(') then
      begin
        TextToSelect := '';
        sLength := 0;
        for Index := Position + 1 to Length(NewText) do
        begin
          if CharInSet(NewText[Index], [' ', ',', ')']) then
          begin
            sLength := Index - Position - 1;
            TextToSelect := Copy(NewText, Position + 1, sLength);
            break;
          end;
        end;
        If TextToSelect <> '' then
        begin
//          NewExpression := jreFormula.Lines.Text;
          Start := Position + Start;
//          Start := PosEx(TextToSelect, NewExpression, Start) -1;
//
          jreFormula.SelStart := Start;
          jreFormula.SelLength := sLength;
//
//          While(jreFormula.SelText <> TextToSelect)
//            and (jreFormula.SelStart + sLength + 1 < Length(NewExpression)) do
//          begin
//            NewStart := jreFormula.SelStart + 1;
//            jreFormula.SelStart := NewStart;
//            jreFormula.SelLength := sLength;
//            if jreFormula.SelStart <> NewStart then
//            begin
//              NewStart := NewStart + 1;
//              jreFormula.SelStart := NewStart;
//              jreFormula.SelLength := sLength;
//              if jreFormula.SelStart <> NewStart then
//              begin
//                break
//              end;
//            end;
//          end
        end;
//      end
//      else
//      begin
//        jreFormula.SelStart := Start - 1 + Length(NewText);
//        jreFormula.SelLength := 0;
      end;
//    end
//    else
//    begin
//      jreFormula.SelText := NewText;
//    end;
  finally
    FocusControl(jreFormula);
  end;
end;

procedure TfrmFmpFormulaEditor.jreFormulaChange(Sender: TObject);
begin
  btnOK.Enabled := True;
  DiagramFormula;
end;

procedure TfrmFmpFormulaEditor.jreFormulaDblClick(Sender: TObject);
type
  TBrace = (bNone, bStart, bStop);
const
  Separators: TSysCharSet = [' ', ',', '(', ')', #10, #13, #9, '*', '/', '+',
  '-', '"'];
  LineBreakLength = 2;
var
  LineNumber: integer;
  AString: string;
  Index: integer;
  Stop, Start: integer;
  function CurlyBrace: TBrace;
  var
    SelectedString: string;
    SelectedChar: char;
  begin
    result := bNone;
    SelectedString := Copy(AString, Start, 1);
    if Length(SelectedString) > 0 then
    begin
      SelectedChar := SelectedString[1];
      if SelectedChar = '{' then
      begin
        result := bStart;
      end
      else if SelectedChar = '}' then
      begin
        result := bStop;
      end;

    end;
  end;
begin
  inherited;
  if FClickSelectionStart >= 0 then
  begin
    AString := jreFormula.Lines.Text;

    LineNumber := 0;
    Index := PosEx(sLineBreak, AString, 1);
    while (Index < FClickSelectionStart) and (Index > 0) do
    begin
      Inc(LineNumber);
      Index := PosEx(sLineBreak, AString, Index+LineBreakLength);
    end;
    FClickSelectionStart := FClickSelectionStart + LineNumber*LineBreakLength;

    Stop := Length(AString);
    Start := 1;

    case CurlyBrace of
      bNone:
        begin
          // Change stop so it represents the location of the first separator
          // after the selected position.
          for Index := FClickSelectionStart + 1 to Length(AString) + 1 do
          begin
            if Index <= Length(AString) then
            begin
              Stop := Index;
              if CharInSet(AString[Index], Separators) then
              begin
                break;
              end;
            end
            else
            begin
              Stop := Length(AString) + 1;
            end;
          end;
          // Change Start so it represents the first character
          // after the first separator before the selected position.
          for Index := FClickSelectionStart downto 1 do
          begin
            if Index <= Length(AString) then
            begin
              Start := Index;
              if CharInSet(AString[Index], Separators) then
              begin
                Start := Start + 1;
                break;
              end;
            end
            else
            begin
              Start := Length(AString);
            end;
          end;
        end;
      bStart:
        begin
          for Index := FClickSelectionStart + 1 to Length(AString) + 1 do
          begin
            // change Stop so that it represents the first
            // end curly brace after the selected position.
            if Index <= Length(AString) then
            begin
              Stop := Index;
              if AString[Index] = '}' then
              begin
                Stop := Stop + 1;
                break;
              end;
            end
            else
            begin
              Stop := Length(AString) + 1;
            end;
          end;
        end;
      bStop:
        begin
            // change Start so that it represents the first
            // start curly brace before the selected position.
          for Index := FClickSelectionStart downto 1 do
          begin
            if Index <= Length(AString) then
            begin
              Start := Index;
              if AString[Index] = '{' then
              begin
                break;
              end;
            end
            else
            begin
              Start := Length(AString);
            end;
          end;
        end;
    else Assert(False);
    end;

    LineNumber := 0;
    Index := PosEx(sLineBreak, AString, 1);
    while (Index < Start) and (Index > 0) do
    begin
      Inc(LineNumber);
      Index := PosEx(sLineBreak, AString, Index+LineBreakLength);
    end;
    Start := Start - LineNumber*LineBreakLength;
    Stop := Stop - LineNumber*LineBreakLength;

    // Set FNewSelectionStart and FNewSelectionLength for use in
    // TimerSetSelection.
    FNewSelectionStart := Start - 1;
    if Stop <> Start then
    begin
      FNewSelectionLength := Stop - Start;
    end
    else
    begin
      FNewSelectionLength := 1;
    end;
    // The selection can't be changed here so do it in TimerSetSelectionO.
    Timer.OnTimer := TimerSetSelection;
    Timer.Enabled := True;
  end;
end;

procedure TfrmFmpFormulaEditor.jreFormulaMouseUp(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FClickSelectionStart := jreFormula.SelStart;
end;

procedure TfrmFmpFormulaEditor.jreFormulaSelectionChange(Sender: TObject);
var
  SelText: string;
  // Use the following variables for TRichEdit
//  PriorSelStart: integer;
//  PriorSelLength: integer;
//  NewSelStart: integer;
//  NewSelLength: integer;
  // Use the following variables for TJvRichEdit
  PriorSelection: TCharRange;
  NewSelection: TCharRange;
begin
  // Use the following for TRichEdit
//  if FUpdating then Exit;
//  FUpdating := True;
//  try
//    PriorSelStart := jreFormula.SelStart;
//    PriorSelLength := jreFormula.SelLength;
//    try
//      if FSetColor then
//      begin
//        jreFormula.SelStart := 0;
//        jreFormula.SelLength := MAXINT;
//        jreFormula.SelAttributes.Color := clBlack;
//        jreFormula.SelStart := PriorSelStart;
//        jreFormula.SelLength := PriorSelLength;
//        FSetColor := False;
//      end;
//
//      NewSelStart := PriorSelStart;
//      NewSelLength := PriorSelLength;
//      if NewSelLength = 0 then
//      begin
//        Inc(NewSelLength);
//      end;
//      jreFormula.SelStart := NewSelStart;
//      jreFormula.SelLength := NewSelLength;
//      SelText := jreFormula.SelText;
//      if SelText = '(' then
//      begin
//        MatchStartingParen(PriorSelStart, PriorSelLength);
//      end
//      else if SelText = ')' then
//      begin
//        MatchEndingParen(PriorSelStart, PriorSelLength);
//      end;
//    finally
//      jreFormula.SelStart := PriorSelStart;
//      jreFormula.SelLength := PriorSelLength;
//    end;
//  finally
//    FUpdating := False;
//  end;

  // Use the following for TJvRichEdit
  if FUpdating then Exit;
  FUpdating := True;
  try
    PriorSelection := jreFormula.GetSelection;
    try
      if FSetColor then
      begin
        jreFormula.SetSelection(0, MAXINT, False);
        jreFormula.SelAttributes.BackColor := clWindow;
        jreFormula.SetSelection(PriorSelection.cpMin, PriorSelection.cpMax, False);
        FSetColor := False;
      end;

      NewSelection := PriorSelection;
      if NewSelection.cpMin = NewSelection.cpMax then
      begin
        Inc(NewSelection.cpMax);
      end;
      SelText := jreFormula.GetTextRange(NewSelection.cpMin, NewSelection.cpMax);
      if SelText = '(' then
      begin
        MatchStartingParen(PriorSelection);
      end
      else if SelText = ')' then
      begin
        MatchEndingParen(PriorSelection);
      end;
    finally
      jreFormula.SetSelection(PriorSelection.cpMin, PriorSelection.cpMax, False);
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TfrmFmpFormulaEditor.MatchEndingParen(PriorSelection: TCharRange);
var
  Levels: TIntegerList;
  Starts: TIntegerList;
  StoredLevel: Integer;
  Level: Integer;
  Index: Integer;
  NewSelection: TCharRange;
  SelText: string;
  InString: boolean;
begin
  // Use for TJvRichEdit

  Levels := TIntegerList.Create;
  Starts := TIntegerList.Create;
  try
    NewSelection.cpMin := 0;
    NewSelection.cpMax := 1;
    StoredLevel := -1;
    Level := 0;
    InString := False;
    for Index := 0 to MAXINT do
    begin
      SelText := jreFormula.GetTextRange(NewSelection.cpMin, NewSelection.cpMax);
//      SelText := jreFormula.SelText;
      if SelText = '' then
      begin
        break;
      end
      else if (SelText = '"') and not InString then
      begin
        InString := True;
      end
      else if SelText = '"' then
      begin
        InString := False;
      end
      else if InString then
      begin
        if NewSelection.cpMin = PriorSelection.cpMin then
        begin
          break;
        end;
      end
      else if (SelText = '(') or (SelText = '[') then
      begin
        Inc(Level);
        Levels.Add(Level);
        Starts.Add(NewSelection.cpMin);
      end
      else if (SelText = ')') or (SelText = ']') then
      begin
        if NewSelection.cpMin = PriorSelection.cpMin then
        begin
          jreFormula.SetSelection(NewSelection.cpMin, NewSelection.cpMax, False);
          jreFormula.SelAttributes.BackColor := clAqua;
          StoredLevel := Level;
          FSetColor := True;
          break;
        end;
        Dec(Level);
      end;
      Inc(NewSelection.cpMin);
      Inc(NewSelection.cpMax);
    end;
    if StoredLevel >= 0 then
    begin
      for Index := Levels.Count - 1 downto 0 do
      begin
        if Levels[Index] = StoredLevel then
        begin
          jreFormula.SetSelection(Starts[Index], Starts[Index] + 1, False);
          jreFormula.SelAttributes.BackColor := clAqua;
          break;
        end;
      end;
    end;
  finally
    Levels.Free;
    Starts.Free;
  end;
end;

procedure TfrmFmpFormulaEditor.MatchStartingParen(PriorSelection: TCharRange);
var
  Index: Integer;
  Level: Integer;
  StoredLevel: Integer;
  SelText: string;
  NewSelection: TCharRange;
  InString: boolean;
begin
  // Use for TJvRichEdit

  NewSelection.cpMin := 0;
  NewSelection.cpMax := 1;
  StoredLevel := -1;
  Level := 0;
  InString := False;
  for Index := 0 to MAXINT do
  begin
    SelText := jreFormula.GetTextRange(NewSelection.cpMin, NewSelection.cpMax);
    jreFormula.SetSelection(NewSelection.cpMin, NewSelection.cpMax, False);
    if SelText = '' then
    begin
      break;
    end
    else if (SelText = '"') and not InString then
    begin
      InString := True;
    end
    else if SelText = '"' then
    begin
      InString := False;
    end
    else if InString then
    begin
      if NewSelection.cpMin = PriorSelection.cpMin then
      begin
        break;
      end;
    end
    else if (SelText = '(') or (SelText = '[') then
    begin
      Inc(Level);
      if NewSelection.cpMin = PriorSelection.cpMin then
      begin
        jreFormula.SetSelection(NewSelection.cpMin, NewSelection.cpMax, False);
        jreFormula.SelAttributes.BackColor := clAqua;
        FSetColor := True;
        StoredLevel := Level;
      end;
    end
    else if (SelText = ')') or (SelText = ']') then
    begin
      if Level = StoredLevel then
      begin
        jreFormula.SetSelection(NewSelection.cpMin, NewSelection.cpMax, False);
        jreFormula.SelAttributes.BackColor := clAqua;
        break;
      end;
      Dec(Level);
    end;
    Inc(NewSelection.cpMin);
    Inc(NewSelection.cpMax);
  end;
end;

procedure TfrmFmpFormulaEditor.SetFormula(const Value: string);
begin
  jreFormula.Lines.Clear;
  jreFormula.Lines.Add(Value);
  jreFormula.SelectAll;
end;

procedure TfrmFmpFormulaEditor.TimerSetSelection(Sender: TObject);
begin
  Timer.Enabled := False;
  jreFormula.SelStart := FNewSelectionStart;
  jreFormula.SelLength := FNewSelectionLength;
end;

procedure TfrmFmpFormulaEditor.tvFormulaDiagramCollapsed(Sender: TObject; Node:
    TTreeNode);
var
  NodeData: TTreeNodeTextStorage;
begin
  inherited;
  NodeData := Node.Data;
  Node.Text := NodeData.ClosedText;
end;

procedure TfrmFmpFormulaEditor.tvFormulaDiagramExpanded(Sender: TObject; Node:
    TTreeNode);
var
  NodeData: TTreeNodeTextStorage;
begin
  inherited;
  NodeData := Node.Data;
  Node.Text := NodeData.OpenText;
end;

procedure TfrmFmpFormulaEditor.tvItemsChange(Sender: TObject; Node: TTreeNode);
var
  IsFunction: boolean;
begin
  inherited;
  FSelectedNode := Node;
  FFunctionHelpString := Node.Text;
  IsFunction := False;
  Assert(FFunctions <> nil);
  while (Node <> nil) and (Node <> FFunctions) do
  begin
    Node := Node.Parent;
    if Node = FFunctions then
    begin
      IsFunction := True;
      break;
    end;
  end;
  btnFunctionHelp.HelpKeyWord := FFunctionHelpString;
  btnFunctionHelp.Enabled := IsFunction;
end;

procedure TfrmFmpFormulaEditor.tvItemsDblClick(Sender: TObject);
var
  AnObject: TObject;
  FClass: TFunctionClass;
  Value: TCustomValue;
  Prototype: string;
  SeparatorPos: integer;
begin
  inherited;
  Assert(FSelectedNode <> nil);
  AnObject := FSelectedNode.Data;
  if AnObject <> nil then
  begin
    if AnObject is TFunctionClass then
    begin
      FClass := FSelectedNode.Data;
      Prototype := FClass.Prototype;
      SeparatorPos := Pos('|', Prototype);
      while SeparatorPos > 0 do
      begin
        Prototype := Copy(Prototype, SeparatorPos + 1, MAXINT);
        SeparatorPos := Pos('|', Prototype);
      end;
      InsertText(Prototype);
    end
    else if AnObject is TCustomValue then
    begin
      Value := FSelectedNode.Data;
      InsertText(Value.DecompileDisplay);
    end
  end;
end;

procedure TfrmFmpFormulaEditor.tvItemsMouseDown(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitTests;
begin
  inherited;
  HitInfo := tvItems.GetHitTestInfoAt(X, Y);
  if (htOnLabel in HitInfo) and (tvItems.Selected <> nil)
    and (tvItems.Selected.Count > 0) then
  begin
    tvItems.Selected.Expanded := not tvItems.Selected.Expanded ;
  end;
end;

procedure TfrmFmpFormulaEditor.UpdateTreeList;
var
  Index: integer;
  FClass: TFunctionClass;
  Prototype: string;
  ChildName: string;
  SeparatorPos: integer;
  ParentNode: TTreeNode;
  ChildNode: TTreeNode;
  FunctionNames: TStringList;
  Item: TSpecialImplementor;
begin
  FunctionNames := TStringList.Create;
  try
    FunctionNames.Sorted := True;
    FunctionNames.Duplicates := dupIgnore;
    for Index := 0 to OverloadedFunctionList.Count - 1 do
    begin
      FClass := OverloadedFunctionList[Index] as TFunctionClass;
      if not FClass.Hidden then
      begin
        FunctionNames.AddObject(FClass.Prototype, FClass);
      end;
    end;
    for Index := 0 to rbFormulaParser.SpecialImplementorList.Count - 1 do
    begin
      Item := rbFormulaParser.SpecialImplementorList[Index];
      FClass := Item.FunctionClass;
      if not FClass.Hidden then
      begin
        FunctionNames.AddObject(FClass.Prototype, FClass);
      end;
    end;
    for Index := 0 to rbFormulaParser.Functions.Count - 1 do
    begin
      FClass := rbFormulaParser.Functions.FunctionClass[Index];
      if not FClass.Hidden then
      begin
        FunctionNames.AddObject(FClass.Prototype, FClass);
      end;
    end;

    CreateNodesForVariables;
//    CreateNodesForTimeSeries;

    FFunctions := tvItems.Items.Add(nil, StrFunctions);
    for Index := 0 to FunctionNames.Count - 1 do
    begin
      ParentNode := FFunctions;
      FClass := FunctionNames.Objects[Index] as TFunctionClass;
      Prototype := FClass.Prototype;
      SeparatorPos := Pos('|', Prototype);
      while SeparatorPos > 0 do
      begin
        ChildName := Copy(Prototype, 1, SeparatorPos - 1);
        Prototype := Copy(Prototype, SeparatorPos + 1, MAXINT);
        ChildNode := ParentNode.getFirstChild;
        while ChildNode <> nil do
        begin
          if ChildNode.Text = ChildName then
          begin
            ParentNode := ChildNode;
            Break;
          end;
          ChildNode := ParentNode.GetNextChild(ChildNode);
        end;
        if ChildNode = nil then
        begin
          ParentNode := tvItems.Items.AddChild(ParentNode, ChildName);
        end;
        SeparatorPos := Pos('|', Prototype);
      end;

      tvItems.Items.AddChildObject(ParentNode, FClass.Name, FClass);
    end;
  finally
    FunctionNames.Free;
  end;
end;

procedure TfrmFmpFormulaEditor.AddNewFunctions;
begin
  rbFormulaParser.Functions.Add(CEILING_Function);
  rbFormulaParser.Functions.Add(FLOOR_Function);
  rbFormulaParser.Functions.Add(LOG_Function);
  rbFormulaParser.Functions.Add(NEG_Function);
  rbFormulaParser.Functions.Add(NEG_TO_ZERO_Function);
  rbFormulaParser.Functions.Add(POS_TO_ZERO_Function);
  rbFormulaParser.Functions.Add(TRUNCATE_Function);
end;

procedure TfrmFmpFormulaEditor.btnOKClick(Sender: TObject);
var
  AFormula: string;
begin
  inherited;
  AFormula := Formula;
  try
    if rbFormulaParser.Compile(AFormula) >= 0 then
    begin
      FResultSet := True;
    end;
    Close;
  except on E: ErbwParserError do
    begin
      btnOK.Enabled := False;
      Beep;
      MessageDlg(E.Message + SLineBreak + StrYouCanCheckTheDo, mtError, [mbOK], 0);
    end;
  end;
  if FResultSet then
  begin
    ModalResult := mrOK
  end;
  if tvItems.Selected <> nil then
  begin
    PriorSelectedText := tvItems.Selected.Text;
  end;
end;

procedure TfrmFmpFormulaEditor.DefineNewOperators;
begin
  DefineAndOperator;
  DefinOrOperator;
  DefineEqualsOperator;
  DefineNotEqualsOperator;
  DefineLessThanOperator;
  DefineGreaterThanOperator;
  DefineGreaterThanOrEqualsOperator;
  DefineLessThanOrEqualsOperator;
end;

procedure TfrmFmpFormulaEditor.FormShow(Sender: TObject);
var
  Index: Integer;
  Item: TTreeNode;
begin
  inherited;
  if PriorSelectedText <> '' then
  begin
    for Index := 0 to tvItems.Items.Count - 1 do
    begin
      Item := tvItems.Items[Index];
      if Item.Text = PriorSelectedText then
      begin
        tvItems.Selected := Item;
        break;
      end;
    end;
  end;
  // For some reason ModelMuse sometimes has trouble reading the ScrollBars
  // property when creating the form.
  jreFormula.ScrollBars := ssVertical;

  jreFormula.SetFocus;
end;

procedure TfrmFmpFormulaEditor.RemoveUnsupportedOrChangedOperators;
begin
  rbFormulaParser.RemoveOperator('and');
  rbFormulaParser.RemoveOperator('or');
  rbFormulaParser.RemoveOperator('not');
  rbFormulaParser.RemoveOperator('xor');
  rbFormulaParser.RemoveOperator('=');
  rbFormulaParser.RemoveOperator('<>');
  rbFormulaParser.RemoveOperator('>');
  rbFormulaParser.RemoveOperator('<');
  rbFormulaParser.RemoveOperator('>=');
  rbFormulaParser.RemoveOperator('<=');
  rbFormulaParser.RemoveOperator('mod');
  rbFormulaParser.RemoveOperator('div');
end;

procedure TfrmFmpFormulaEditor.RemoveUnsupportedFunctions;
begin
  rbFormulaParser.Functions.Remove('case');
  rbFormulaParser.Functions.Remove('caseb');
  rbFormulaParser.Functions.Remove('casei');
  rbFormulaParser.Functions.Remove('caser');
  rbFormulaParser.Functions.Remove('caset');
  rbFormulaParser.Functions.Remove('ifb');
  rbFormulaParser.Functions.Remove('ifi');
  rbFormulaParser.Functions.Remove('ifr');
  rbFormulaParser.Functions.Remove('ift');
  rbFormulaParser.Functions.Remove('absi');
  rbFormulaParser.Functions.Remove('absr');
  rbFormulaParser.Functions.Remove('closest');
  rbFormulaParser.Functions.Remove('distance');
  rbFormulaParser.Functions.Remove('factoriali');
  rbFormulaParser.Functions.Remove('factorialr');
  rbFormulaParser.Functions.Remove('frac');
  rbFormulaParser.Functions.Remove('interpolate');
  rbFormulaParser.Functions.Remove('intpower');
  rbFormulaParser.Functions.Remove('log');
  rbFormulaParser.Functions.Remove('ln');
  rbFormulaParser.Functions.Remove('maxi');
  rbFormulaParser.Functions.Remove('maxr');
  rbFormulaParser.Functions.Remove('mini');
  rbFormulaParser.Functions.Remove('minr');
  rbFormulaParser.Functions.Remove('multiinterpolate');
  rbFormulaParser.Functions.Remove('odd');
  rbFormulaParser.Functions.Remove('pi');
  rbFormulaParser.Functions.Remove('power');
  rbFormulaParser.Functions.Remove('sqr');
  rbFormulaParser.Functions.Remove('sqri');
  rbFormulaParser.Functions.Remove('sqrr');
  rbFormulaParser.Functions.Remove('trunc');
  rbFormulaParser.Functions.Remove('copy');
  rbFormulaParser.Functions.Remove('floattotext');
  rbFormulaParser.Functions.Remove('inttotext');
  rbFormulaParser.Functions.Remove('length');
  rbFormulaParser.Functions.Remove('lowercase');
  rbFormulaParser.Functions.Remove('pos');
  rbFormulaParser.Functions.Remove('Posex');
  rbFormulaParser.Functions.Remove('positininlist');
  rbFormulaParser.Functions.Remove('texttofloat');
  rbFormulaParser.Functions.Remove('texttofloatdef');
  rbFormulaParser.Functions.Remove('texttoint');
  rbFormulaParser.Functions.Remove('texttointdef');
  rbFormulaParser.Functions.Remove('trim');
  rbFormulaParser.Functions.Remove('uppercase');
  rbFormulaParser.Functions.Remove('arccos');
  rbFormulaParser.Functions.Remove('arccosh');
  rbFormulaParser.Functions.Remove('arcsin');
  rbFormulaParser.Functions.Remove('arcsinh');
  rbFormulaParser.Functions.Remove('arctan');
  rbFormulaParser.Functions.Remove('arctanh');
  rbFormulaParser.Functions.Remove('cos');
  rbFormulaParser.Functions.Remove('cosh');
  rbFormulaParser.Functions.Remove('degtorad');
  rbFormulaParser.Functions.Remove('radtodeg');
  rbFormulaParser.Functions.Remove('sin');
  rbFormulaParser.Functions.Remove('sinh');
  rbFormulaParser.Functions.Remove('tan');
  rbFormulaParser.Functions.Remove('tanh');
end;

procedure TfrmFmpFormulaEditor.SplitterCanResize(Sender: TObject; var NewSize:
    Integer; var Accept: Boolean);
begin
  inherited;
  Accept := NewSize >= pnlMain.Constraints.MinWidth;
end;

function _And(Values: array of pointer): Boolean;
begin
  result := PBoolean(Values[0])^ and PBoolean(Values[1])^;
end;

function _Or(Values: array of pointer): Boolean;
begin
  result := PBoolean(Values[0])^ or PBoolean(Values[1])^;
end;

function _EqualRR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ = PDouble(Values[1])^;
end;

function _EqualRI(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ = PInteger(Values[1])^;
end;

function _EqualIR(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ = PDouble(Values[1])^;
end;

function _EqualI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ = PInteger(Values[1])^;
end;

function _EqualB(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[0])^ = PBoolean(Values[1])^;
end;

function _NotEqualB(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[0])^ <> PBoolean(Values[1])^;
end;

function _NotEqualI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ <> PInteger(Values[1])^;
end;

function _NotEqualRR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ <> PDouble(Values[1])^;
end;

function _NotEqualRI(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ <> PInteger(Values[1])^;
end;

function _NotEqualIR(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ <> PDouble(Values[1])^;
end;

function _LessThanI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ < PInteger(Values[1])^;
end;

function _LessThanRR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ < PDouble(Values[1])^;
end;

function _LessThanRI(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ < PInteger(Values[1])^;
end;

function _LessThanIR(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ < PDouble(Values[1])^;
end;

function _GreaterThanI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ > PInteger(Values[1])^;
end;

function _GreaterThanRR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ > PDouble(Values[1])^;
end;

function _GreaterThanRI(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ > PInteger(Values[1])^;
end;

function _GreaterThanIR(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ > PDouble(Values[1])^;
end;

function _LessThanOrEqualsI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ <= PInteger(Values[1])^;
end;

function _LessThanOrEqualsRR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ <= PDouble(Values[1])^;
end;

function _LessThanOrEqualsRI(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ <= PInteger(Values[1])^;
end;

function _LessThanOrEqualsIR(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ <= PDouble(Values[1])^;
end;

function _GreaterThanOrEqualsI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ >= PInteger(Values[1])^;
end;

function _GreaterThanOrEqualsRR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ >= PDouble(Values[1])^;
end;

function _GreaterThanOrEqualsRI(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ >= PInteger(Values[1])^;
end;
function _GreaterThanOrEqualsIR(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ >= PDouble(Values[1])^;
end;

function _Ceiling(Values: array of pointer): double;
begin
  result := Ceil(PDouble(Values[0])^);
end;

function _Floor(Values: array of pointer): double;
begin
  result := Floor(PDouble(Values[0])^);
end;

function _Log(Values: array of pointer): double;
begin
  result := Ln(PDouble(Values[0])^);
end;

function _Neg(Values: array of pointer): double;
begin
  result := -(PDouble(Values[0])^);
end;

function _NegToZero(Values: array of pointer): double;
begin
  result := Max(0,(PDouble(Values[0])^));
end;

function _PosToZero(Values: array of pointer): double;
begin
  result := Min(0,(PDouble(Values[0])^));
end;

function _Truncate(Values: array of pointer): double;
begin
  result := Trunc(PDouble(Values[0])^);
end;


resourcestring
  StrMath = 'Math|';
  StrLogical = 'Logical|';


procedure CreateFunctionClasses;
begin
  AndOperator := TFunctionClass.Create;
  AndOperator.AllowConversionToConstant := True;
  AndOperator.InputDataCount := 2;
  AndOperator.InputDataTypes[0] := rdtBoolean;
  AndOperator.InputDataTypes[1] := rdtBoolean;
  AndOperator.Name := '&';
  AndOperator.Prototype := StrLogical + '&';
  AndOperator.OptionalArguments := 0;
  AndOperator.BFunctionAddr := _And;

  OrOperator := TFunctionClass.Create;
  OrOperator.AllowConversionToConstant := True;
  OrOperator.InputDataCount := 2;
  OrOperator.InputDataTypes[0] := rdtBoolean;
  OrOperator.InputDataTypes[1] := rdtBoolean;
  OrOperator.Name := '|';
  OrOperator.Prototype := StrLogical + '|';
  OrOperator.OptionalArguments := 0;
  OrOperator.BFunctionAddr := _Or;

  EqualIOperator := TFunctionClass.Create;
//  OperatorList.Add(EqualIOperator);
  EqualIOperator.InputDataCount := 2;
  EqualIOperator.BFunctionAddr := @_EqualI;
  EqualIOperator.Name := '==';
  EqualIOperator.Prototype := StrLogical+'=';
  EqualIOperator.InputDataTypes[0] := rdtInteger;
  EqualIOperator.InputDataTypes[1] := rdtInteger;
  EqualIOperator.OptionalArguments := 0;

  EqualRROperator := TFunctionClass.Create;
  EqualRROperator.InputDataCount := 2;
  EqualRROperator.BFunctionAddr := @_EqualRR;
  EqualRROperator.Name := '==';
  EqualRROperator.Prototype := StrLogical+'=';
  EqualRROperator.InputDataTypes[0] := rdtDouble;
  EqualRROperator.InputDataTypes[1] := rdtDouble;
  EqualRROperator.OptionalArguments := 0;

  EqualRIOperator := TFunctionClass.Create;
  EqualRIOperator.InputDataCount := 2;
  EqualRIOperator.BFunctionAddr := @_EqualRI;
  EqualRIOperator.Name := '==';
  EqualRIOperator.Prototype := StrLogical+'=';
  EqualRIOperator.InputDataTypes[0] := rdtDouble;
  EqualRIOperator.InputDataTypes[1] := rdtInteger;
  EqualRIOperator.OptionalArguments := 0;

  EqualIROperator := TFunctionClass.Create;
  EqualIROperator.InputDataCount := 2;
  EqualIROperator.BFunctionAddr := @_EqualIR;
  EqualIROperator.Name := '==';
  EqualIROperator.Prototype := StrLogical+'=';
  EqualIROperator.InputDataTypes[0] := rdtInteger;
  EqualIROperator.InputDataTypes[1] := rdtDouble;
  EqualIROperator.OptionalArguments := 0;

  EqualBOperator := TFunctionClass.Create;
  EqualBOperator.InputDataCount := 2;
  EqualBOperator.BFunctionAddr := @_EqualB;
  EqualBOperator.Name := '==';
  EqualBOperator.Prototype := StrLogical+'=';
  EqualBOperator.InputDataTypes[0] := rdtBoolean;
  EqualBOperator.InputDataTypes[1] := rdtBoolean;
  EqualBOperator.OptionalArguments := 0;

  NotEqualIOperator := TFunctionClass.Create;
//  OperatorList.Add(NotEqualIOperator);
  NotEqualIOperator.InputDataCount := 2;
  NotEqualIOperator.BFunctionAddr := @_NotEqualI;
  NotEqualIOperator.Name := '!=';
  NotEqualIOperator.Prototype := StrLogical+'<>';
  NotEqualIOperator.InputDataTypes[0] := rdtInteger;
  NotEqualIOperator.InputDataTypes[1] := rdtInteger;
  NotEqualIOperator.OptionalArguments := 0;

  NotEqualRROperator := TFunctionClass.Create;
//  OperatorList.Add(NotEqualRROperator);
  NotEqualRROperator.InputDataCount := 2;
  NotEqualRROperator.BFunctionAddr := @_NotEqualRR;
  NotEqualRROperator.Name := '!=';
  NotEqualRROperator.Prototype := StrLogical+'<>';
  NotEqualRROperator.InputDataTypes[0] := rdtDouble;
  NotEqualRROperator.InputDataTypes[1] := rdtDouble;
  NotEqualRROperator.OptionalArguments := 0;

  NotEqualRIOperator := TFunctionClass.Create;
//  OperatorList.Add(NotEqualRIOperator);
  NotEqualRIOperator.InputDataCount := 2;
  NotEqualRIOperator.BFunctionAddr := @_NotEqualRI;
  NotEqualRIOperator.Name := '!=';
  NotEqualRIOperator.Prototype := StrLogical+'<>';
  NotEqualRIOperator.InputDataTypes[0] := rdtDouble;
  NotEqualRIOperator.InputDataTypes[1] := rdtInteger;
  NotEqualRIOperator.OptionalArguments := 0;

  NotEqualIROperator := TFunctionClass.Create;
//  OperatorList.Add(NotEqualIROperator);
  NotEqualIROperator.InputDataCount := 2;
  NotEqualIROperator.BFunctionAddr := @_NotEqualIR;
  NotEqualIROperator.Name := '!=';
  NotEqualIROperator.Prototype := StrLogical+'<>';
  NotEqualIROperator.InputDataTypes[0] := rdtInteger;
  NotEqualIROperator.InputDataTypes[1] := rdtDouble;
  NotEqualIROperator.OptionalArguments := 0;

  NotEqualBOperator := TFunctionClass.Create;
//  OperatorList.Add(NotEqualBOperator);
  NotEqualBOperator.InputDataCount := 2;
  NotEqualBOperator.BFunctionAddr := @_NotEqualB;
  NotEqualBOperator.Name := '!=';
  NotEqualBOperator.Prototype := StrLogical+'<>';
  NotEqualBOperator.InputDataTypes[0] := rdtBoolean;
  NotEqualBOperator.InputDataTypes[1] := rdtBoolean;
  NotEqualBOperator.OptionalArguments := 0;

  LessThanIOperator := TFunctionClass.Create;
//  OperatorList.Add(LessThanIOperator);
  LessThanIOperator.InputDataCount := 2;
  LessThanIOperator.BFunctionAddr := @_LessThanI;
  LessThanIOperator.Name := '<';
  LessThanIOperator.Prototype := StrLogical+'<';
  LessThanIOperator.InputDataTypes[0] := rdtInteger;
  LessThanIOperator.InputDataTypes[1] := rdtInteger;
  LessThanIOperator.OptionalArguments := 0;

  LessThanRROperator := TFunctionClass.Create;
  LessThanRROperator.InputDataCount := 2;
  LessThanRROperator.BFunctionAddr := @_LessThanRR;
  LessThanRROperator.Name := '<';
  LessThanRROperator.Prototype := StrLogical+'<';
  LessThanRROperator.InputDataTypes[0] := rdtDouble;
  LessThanRROperator.InputDataTypes[1] := rdtDouble;
  LessThanRROperator.OptionalArguments := 0;

  LessThanRIOperator := TFunctionClass.Create;
  LessThanRIOperator.InputDataCount := 2;
  LessThanRIOperator.BFunctionAddr := @_LessThanRI;
  LessThanRIOperator.Name := '<';
  LessThanRIOperator.Prototype := StrLogical+'<';
  LessThanRIOperator.InputDataTypes[0] := rdtDouble;
  LessThanRIOperator.InputDataTypes[1] := rdtInteger;
  LessThanRIOperator.OptionalArguments := 0;

  LessThanIROperator := TFunctionClass.Create;
  LessThanIROperator.InputDataCount := 2;
  LessThanIROperator.BFunctionAddr := @_LessThanIR;
  LessThanIROperator.Name := '<';
  LessThanIROperator.Prototype := StrLogical+'<';
  LessThanIROperator.InputDataTypes[0] := rdtInteger;
  LessThanIROperator.InputDataTypes[1] := rdtDouble;
  LessThanIROperator.OptionalArguments := 0;


  GreaterThanIOperator := TFunctionClass.Create;
  GreaterThanIOperator.InputDataCount := 2;
  GreaterThanIOperator.BFunctionAddr := @_GreaterThanI;
  GreaterThanIOperator.Name := '>';
  GreaterThanIOperator.Prototype := StrLogical+'>';
  GreaterThanIOperator.InputDataTypes[0] := rdtInteger;
  GreaterThanIOperator.InputDataTypes[1] := rdtInteger;
  GreaterThanIOperator.OptionalArguments := 0;

  GreaterThanRROperator := TFunctionClass.Create;
  GreaterThanRROperator.InputDataCount := 2;
  GreaterThanRROperator.BFunctionAddr := @_GreaterThanRR;
  GreaterThanRROperator.Name := '>';
  GreaterThanRROperator.Prototype := StrLogical+'>';
  GreaterThanRROperator.InputDataTypes[0] := rdtDouble;
  GreaterThanRROperator.InputDataTypes[1] := rdtDouble;
  GreaterThanRROperator.OptionalArguments := 0;

  GreaterThanRIOperator := TFunctionClass.Create;
  GreaterThanRIOperator.InputDataCount := 2;
  GreaterThanRIOperator.BFunctionAddr := @_GreaterThanRI;
  GreaterThanRIOperator.Name := '>';
  GreaterThanRIOperator.Prototype := StrLogical+'>';
  GreaterThanRIOperator.InputDataTypes[0] := rdtDouble;
  GreaterThanRIOperator.InputDataTypes[1] := rdtInteger;
  GreaterThanRIOperator.OptionalArguments := 0;

  GreaterThanIROperator := TFunctionClass.Create;
  GreaterThanIROperator.InputDataCount := 2;
  GreaterThanIROperator.BFunctionAddr := @_GreaterThanIR;
  GreaterThanIROperator.Name := '>';
  GreaterThanIROperator.Prototype := StrLogical+'>';
  GreaterThanIROperator.InputDataTypes[0] := rdtInteger;
  GreaterThanIROperator.InputDataTypes[1] := rdtDouble;
  GreaterThanIROperator.OptionalArguments := 0;


  LessThanOrEqualsIOperator := TFunctionClass.Create;
  LessThanOrEqualsIOperator.InputDataCount := 2;
  LessThanOrEqualsIOperator.BFunctionAddr := @_LessThanOrEqualsI;
  LessThanOrEqualsIOperator.Name := '<=';
  LessThanOrEqualsIOperator.Prototype := StrLogical+'<=';
  LessThanOrEqualsIOperator.InputDataTypes[0] := rdtInteger;
  LessThanOrEqualsIOperator.InputDataTypes[1] := rdtInteger;
  LessThanOrEqualsIOperator.OptionalArguments := 0;

  LessThanOrEqualsRROperator := TFunctionClass.Create;
  LessThanOrEqualsRROperator.InputDataCount := 2;
  LessThanOrEqualsRROperator.BFunctionAddr := @_LessThanOrEqualsRR;
  LessThanOrEqualsRROperator.Name := '<=';
  LessThanOrEqualsRROperator.Prototype := StrLogical+'<=';
  LessThanOrEqualsRROperator.InputDataTypes[0] := rdtDouble;
  LessThanOrEqualsRROperator.InputDataTypes[1] := rdtDouble;
  LessThanOrEqualsRROperator.OptionalArguments := 0;

  LessThanOrEqualsRIOperator := TFunctionClass.Create;
  LessThanOrEqualsRIOperator.InputDataCount := 2;
  LessThanOrEqualsRIOperator.BFunctionAddr := @_LessThanOrEqualsRI;
  LessThanOrEqualsRIOperator.Name := '<=';
  LessThanOrEqualsRIOperator.Prototype := StrLogical+'<=';
  LessThanOrEqualsRIOperator.InputDataTypes[0] := rdtDouble;
  LessThanOrEqualsRIOperator.InputDataTypes[1] := rdtInteger;
  LessThanOrEqualsRIOperator.OptionalArguments := 0;

  LessThanOrEqualsIROperator := TFunctionClass.Create;
  LessThanOrEqualsIROperator.InputDataCount := 2;
  LessThanOrEqualsIROperator.BFunctionAddr := @_LessThanOrEqualsIR;
  LessThanOrEqualsIROperator.Name := '<=';
  LessThanOrEqualsIROperator.Prototype := StrLogical+'<=';
  LessThanOrEqualsIROperator.InputDataTypes[0] := rdtInteger;
  LessThanOrEqualsIROperator.InputDataTypes[1] := rdtDouble;
  LessThanOrEqualsIROperator.OptionalArguments := 0;


  GreaterThanOrEqualsIOperator := TFunctionClass.Create;
  GreaterThanOrEqualsIOperator.InputDataCount := 2;
  GreaterThanOrEqualsIOperator.BFunctionAddr := @_GreaterThanOrEqualsI;
  GreaterThanOrEqualsIOperator.Name := '>=';
  GreaterThanOrEqualsIOperator.Prototype := StrLogical+'>=';
  GreaterThanOrEqualsIOperator.InputDataTypes[0] := rdtInteger;
  GreaterThanOrEqualsIOperator.InputDataTypes[1] := rdtInteger;
  GreaterThanOrEqualsIOperator.OptionalArguments := 0;

  GreaterThanOrEqualsRROperator := TFunctionClass.Create;
  GreaterThanOrEqualsRROperator.InputDataCount := 2;
  GreaterThanOrEqualsRROperator.BFunctionAddr := @_GreaterThanOrEqualsRR;
  GreaterThanOrEqualsRROperator.Name := '>=';
  GreaterThanOrEqualsRROperator.Prototype := StrLogical+'>=';
  GreaterThanOrEqualsRROperator.InputDataTypes[0] := rdtDouble;
  GreaterThanOrEqualsRROperator.InputDataTypes[1] := rdtDouble;
  GreaterThanOrEqualsRROperator.OptionalArguments := 0;

  GreaterThanOrEqualsRIOperator := TFunctionClass.Create;
  GreaterThanOrEqualsRIOperator.InputDataCount := 2;
  GreaterThanOrEqualsRIOperator.BFunctionAddr := @_GreaterThanOrEqualsRI;
  GreaterThanOrEqualsRIOperator.Name := '>=';
  GreaterThanOrEqualsRIOperator.Prototype := StrLogical+'>=';
  GreaterThanOrEqualsRIOperator.InputDataTypes[0] := rdtDouble;
  GreaterThanOrEqualsRIOperator.InputDataTypes[1] := rdtInteger;
  GreaterThanOrEqualsRIOperator.OptionalArguments := 0;

  GreaterThanOrEqualsIROperator := TFunctionClass.Create;
  GreaterThanOrEqualsIROperator.InputDataCount := 2;
  GreaterThanOrEqualsIROperator.BFunctionAddr := @_GreaterThanOrEqualsIR;
  GreaterThanOrEqualsIROperator.Name := '>=';
  GreaterThanOrEqualsIROperator.Prototype := StrLogical+'>=';
  GreaterThanOrEqualsIROperator.InputDataTypes[0] := rdtInteger;
  GreaterThanOrEqualsIROperator.InputDataTypes[1] := rdtDouble;
  GreaterThanOrEqualsIROperator.OptionalArguments := 0;

end;

procedure DefineFmpFunctions;
begin
  CEILING_Function.ResultType := rdtDouble;
  CEILING_Function.RFunctionAddr := _Ceiling;
  SetLength(CEILING_Function.InputDataTypes, 1);
  CEILING_Function.InputDataTypes[0] := rdtDouble;
  CEILING_Function.OptionalArguments := 0;
  CEILING_Function.CanConvertToConstant := False;
  CEILING_Function.Name := 'CEILING';
  CEILING_Function.Prototype := 'CEILING(Value)';

  FLOOR_Function.ResultType := rdtDouble;
  FLOOR_Function.RFunctionAddr := _Floor;
  SetLength(FLOOR_Function.InputDataTypes, 1);
  FLOOR_Function.InputDataTypes[0] := rdtDouble;
  FLOOR_Function.OptionalArguments := 0;
  FLOOR_Function.CanConvertToConstant := False;
  FLOOR_Function.Name := 'FLOOR';
  FLOOR_Function.Prototype := 'FLOOR(Value)';

  LOG_Function.ResultType := rdtDouble;
  LOG_Function.RFunctionAddr := _Log;
  SetLength(LOG_Function.InputDataTypes, 1);
  LOG_Function.InputDataTypes[0] := rdtDouble;
  LOG_Function.OptionalArguments := 0;
  LOG_Function.CanConvertToConstant := False;
  LOG_Function.Name := 'LOG';
  LOG_Function.Prototype := 'LOG(Value)';

  NEG_Function.ResultType := rdtDouble;
  NEG_Function.RFunctionAddr := _Neg;
  SetLength(NEG_Function.InputDataTypes, 1);
  NEG_Function.InputDataTypes[0] := rdtDouble;
  NEG_Function.OptionalArguments := 0;
  NEG_Function.CanConvertToConstant := False;
  NEG_Function.Name := 'NEG';
  NEG_Function.Prototype := 'NEG(Value)';

  NEG_TO_ZERO_Function.ResultType := rdtDouble;
  NEG_TO_ZERO_Function.RFunctionAddr := _NegToZero;
  SetLength(NEG_TO_ZERO_Function.InputDataTypes, 1);
  NEG_TO_ZERO_Function.InputDataTypes[0] := rdtDouble;
  NEG_TO_ZERO_Function.OptionalArguments := 0;
  NEG_TO_ZERO_Function.CanConvertToConstant := False;
  NEG_TO_ZERO_Function.Name := 'NEG_TO_ZERO';
  NEG_TO_ZERO_Function.Prototype := 'NEG_TO_ZERO(Value)';

  POS_TO_ZERO_Function.ResultType := rdtDouble;
  POS_TO_ZERO_Function.RFunctionAddr := _PosToZero;
  SetLength(POS_TO_ZERO_Function.InputDataTypes, 1);
  POS_TO_ZERO_Function.InputDataTypes[0] := rdtDouble;
  POS_TO_ZERO_Function.OptionalArguments := 0;
  POS_TO_ZERO_Function.CanConvertToConstant := False;
  POS_TO_ZERO_Function.Name := 'POS_TO_ZERO';
  POS_TO_ZERO_Function.Prototype := 'POS_TO_ZERO(Value)';

  TRUNCATE_Function.ResultType := rdtDouble;
  TRUNCATE_Function.RFunctionAddr := _Truncate;
  SetLength(TRUNCATE_Function.InputDataTypes, 1);
  TRUNCATE_Function.InputDataTypes[0] := rdtDouble;
  TRUNCATE_Function.OptionalArguments := 0;
  TRUNCATE_Function.CanConvertToConstant := False;
  TRUNCATE_Function.Name := 'TRUNCATE';
  TRUNCATE_Function.Prototype := 'TRUNCATE(Value)';

end;

initialization

  CreateFunctionClasses;
  DefineFmpFunctions;

finalization

  AndOperator.Free;
  OrOperator.Free;
  EqualRROperator.Free;
  EqualRIOperator.Free;
  EqualIROperator.Free;
  EqualIOperator.Free;
  EqualBOperator.Free;
  NotEqualRROperator.Free;
  NotEqualRIOperator.Free;
  NotEqualIROperator.Free;
  NotEqualIOperator.Free;
  NotEqualBOperator.Free;

  LessThanIOperator.Free;
  LessThanRROperator.Free;
  LessThanRIOperator.Free;
  LessThanIROperator.Free;
  GreaterThanIOperator.Free;
  GreaterThanRROperator.Free;
  GreaterThanRIOperator.Free;
  GreaterThanIROperator.Free;

  LessThanOrEqualsIOperator.Free;
  LessThanOrEqualsRROperator.Free;
  LessThanOrEqualsRIOperator.Free;
  LessThanOrEqualsIROperator.Free;

  GreaterThanOrEqualsIOperator.Free;
  GreaterThanOrEqualsRROperator.Free;
  GreaterThanOrEqualsRIOperator.Free;
  GreaterThanOrEqualsIROperator.Free;

end.
