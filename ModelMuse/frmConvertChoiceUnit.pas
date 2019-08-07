{@abstract(The main purpose of @name is to define @link(TfrmConvertChoice)
  which is used to allow the user to correct problems with formulas whose
  result is of the wrong type for the @link(TDataArray) to which they apply.)}
unit frmConvertChoiceUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls, RbwParser, GoPhastTypes;

type
  {@abstract(An instance of @name is displayed when the user created a formula
    in the Data Sets dialog box (@link(TfrmDataSets)) that gives results
    which are of the wrong type for that @link(TDataArray).  @name gives the
    user an opportunity to decide how to resolve the problem.)}
  TfrmConvertChoice = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking the @name button closes the dialog box
    // and sets the modal result to mrCancel.  The code that created
    // the @classname should respond by doing nothing.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking the @name button causes help for @classname to be displayed.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // Clicking the @name button closes the dialog box
    // and sets the modal result to mrOK.  The code that created
    // the @classname should respond by not changing the formula or data
    // type of the data set.
    // See @link(AdjustTheFormula) and @link(AdjustFormula).
    btnOK: TBitBtn;
    // @name: TLabel;
    // @name displays invariant text in the dialog box.
    Label1: TLabel;
    // @name: TLabel;
    // @name displays invariant text in the dialog box.
    Label2: TLabel;
    // @name: TLabel;
    // @name displays invariant text in the dialog box.
    Label3: TLabel;
    // @name: TLabel;
    // @name displays invariant text in the dialog box.
    Label4: TLabel;
    // @name: TLabel;
    // @name displays the type of data contained in the data set.
    lblDataSetDataType: TLabel;
    // @name: TLabel;
    // @name displays the type of data returned by the formula.
    lblFormulaDataType: TLabel;
    // @name: TLabel;
    // @name displays the name of the data set that has an incorrect formula.
    lblVariableName: TLabel;
    // @name: TPanel;
    // @name holds the buttons at the bottom of the dialog box.
    pnlButton: TPanel;
    // @name: TPanel;
    // @name holds the text at the top of the dialog box
    pnlTop: TPanel;
    // @name: TRadioGroup;
    // @name displays the possible ways of dealing with the mismatch between
    // the formula and the data set..
    rgChoice: TRadioGroup;
  private
    // @name: TRbwDataType;
    // @name is the type of data stored in the data set.
    FDataSetDataType: TRbwDataType;
    // @name: TRbwDataType;
    // @name is the type of data returned by the formula.
    FFormulaDataType: TRbwDataType;
    { Private declarations }
  public
    // @name is called by the code that detects the problem (See
    // @link(TfrmConvertChoice)) to transfer information about the problem
    // to the @classname.
    // @param(VariableName VariableName is the name of the
    //    @link(TDataArray) whose formula is
    //    incorrect.)
    // @param(AFormulaDataType AFormulaDataType indicates
    //    the type of data returned by the
    //    formula for the @link(TDataArray).)
    // @param(ADataSetDataType ADataSetDataType indicates
    //    the type of data stored by the
    //    @link(TDataArray).)
    // @param(DefaultChoice DefaultChoice indicates
    //    the choice in @link(rgChoice) to be
    //    presented to the user as the default response.)
    // @param(Lock indicates which aspects of the @link(TDataArray) are locked
    //    and can not be changed. Some possible responses can be disabled based
    //    on Lock.)
    procedure GetData(const VariableName: string;
      const AFormulaDataType, ADataSetDataType: TRbwDataType; const
      DefaultChoice: integer; const Lock: TDataLock);
    // @name converts OldFormula to a formula that returns data
    // of the correct type.
    // @link(GetData) must be called before calling @name in order to
    // specify the type of data returned by the formula and the
    // type of data required by the @link(TDataArray).
    // @name calls @link(AdjustFormula).
    function AdjustTheFormula(const OldFormula: string): string;
    { Public declarations }
  end;

// @name converts OldFormula to a valid formula that returns data
// of the correct type for a @link(TDataArray).
// @param(OldFormula OldFormula is the existing (or proposed) formula for the
//    @link(TDataArray).)
// @param(FormulaDataType FormulaDataType indicates
//    the type of data returned by the
//    formula for the @link(TDataArray).)
// @param(DataSetDataType DataSetDataType indicates
//    the type of data stored by the @link(TDataArray).)
// @returns(@name returns a copy of OldFormula modified so that it returns
//    data that matches DataSetDataType.)
function AdjustFormula(const OldFormula: string;
  const FormulaDataType, DataSetDataType: TRbwDataType): string;

implementation

{$R *.dfm}

function AdjustFormula(const OldFormula: string;
  const FormulaDataType, DataSetDataType: TRbwDataType): string;
begin
  // OldFormula is a string that can be compiled by a TRbwParser to
  // a TExpression.
  // FormulaDataType indicates the ResultType of the TExpression created
  // by compiling OldFormula.
  // DataSetDataType indicates the DataType of the TDataArray for which this
  // formula should be used.

  // This function returns a new string that will ideally be
  // compileable by a TRbwParser to a TExpression that has a ResultType
  // of DataSetDataType.  It will thus be possible to use the new
  // formula to assign values to the desired TDataArray.
  if (DataSetDataType = FormulaDataType) or
    ((DataSetDataType = rdtDouble) and (FormulaDataType = rdtInteger)) then
  begin
    result := OldFormula;
  end
  else
  begin
    case DataSetDataType of
      rdtDouble:
        begin
          case FormulaDataType of
            rdtDouble:
              begin
                Assert(False);
              end;
            rdtInteger:
              begin
                Assert(False);
              end;
            rdtBoolean:
              begin
                result := 'If(' + OldFormula + ', 1., 0.)';
              end;
            rdtString:
              begin
                result := 'StrToFloatDef(' + OldFormula + ', 0.)';
              end;
          else
            Assert(False);
          end;
        end;
      rdtInteger:
        begin
          case FormulaDataType of
            rdtDouble:
              begin
                result := 'Round(' + OldFormula + ')';
              end;
            rdtInteger:
              begin
                Assert(False);
              end;
            rdtBoolean:
              begin
                result := 'If(' + OldFormula + ', 1, 0)';
              end;
            rdtString:
              begin
                result := 'StrToIntDef(' + OldFormula + ', 0)';
              end;
          else
            Assert(False);
          end;
        end;
      rdtBoolean:
        begin
          case FormulaDataType of
            rdtDouble:
              begin
                result := OldFormula + ' <> 0.';
              end;
            rdtInteger:
              begin
                result := OldFormula + ' <> 0';
              end;
            rdtBoolean:
              begin
                Assert(False);
              end;
            rdtString:
              begin
                result := '(LowerCase(' + OldFormula + ') = "true")';
              end;
          else
            Assert(False);
          end;
        end;
      rdtString:
        begin
          case FormulaDataType of
            rdtDouble:
              begin
                result := 'FloatToStr(' + OldFormula + ')';
              end;
            rdtInteger:
              begin
                result := 'IntToStr(' + OldFormula + ')';
              end;
            rdtBoolean:
              begin
                result := 'If(' + OldFormula + ', "True", "False")';
              end;
            rdtString:
              begin
                Assert(False);
              end;
          else
            Assert(False);
          end;
        end;
    else
      Assert(False);
    end;
  end;
end;

{ TfrmConvertChoice }

function TfrmConvertChoice.AdjustTheFormula(const OldFormula: string): string;
begin
  result := AdjustFormula(OldFormula, FFormulaDataType,
    FDataSetDataType);
end;

procedure TfrmConvertChoice.GetData(const VariableName: string;
  const AFormulaDataType, ADataSetDataType: TRbwDataType; const DefaultChoice:
  integer; const Lock: TDataLock);
begin
  FFormulaDataType := AFormulaDataType;
  FDataSetDataType := ADataSetDataType;
  lblVariableName.Caption := VariableName + ':';
  lblFormulaDataType.Caption := '(' + DataTypeToString(AFormulaDataType) + ')';
  lblDataSetDataType.Caption := '(' + DataTypeToString(ADataSetDataType) + ')';
  rgChoice.ItemIndex := DefaultChoice;
  if dcType in Lock then
  begin
    // This will cause TCustomRadioGroup.UpdateButtons to be called.
    rgChoice.WordWrap := not rgChoice.WordWrap;
    rgChoice.WordWrap := not rgChoice.WordWrap;
    rgChoice.Handle;
    rgChoice.Buttons[0].Enabled := False;
//    rgChoice.Controls[0].Enabled := False;
  end;
end;

end.

