{
May 18, 2006: Fixed bug that would cause access violations if an expression
  could not be parsed properly. 
}

{ @abstract(The @name unit declares @Link(TRbwParser) along with
associated classes and types.)}

unit RbwParser;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


{$ifdef CONDITIONALEXPRESSIONS}
  {$if CompilerVersion>=20}
    {$DEFINE Delphi_2009_UP}
  {$ifend}
  {$if CompilerVersion>=21}
    {$DEFINE Delphi_XE_UP}
  {$ifend}
{$endif}

{$ifdef FPC}
    { $DEFINE Delphi_2009_UP}
    {$DEFINE Delphi_XE_UP}
{$endif}


uses
{$IFDEF MSWINDOWS}
  // Indcluding Windows, allows AnsiCompareStr to be inlined with Delphi 2005.
  Windows,
{$ENDIF}
  Types, SysUtils, Classes, Contnrs;

type
  {
    @Name is the prototype for functions in @link(TRbwParser) that
    return a real number.  To use a @link(TRbwRealFunction), assign it to a
    @link(TFunctionRecord.RFunctionAddr), assign the remainder of the fields
    in the @link(TFunctionRecord) and then call @link(TFunctionStringList.Add)
    of @Link(TRbwParser.Functions).
    Be sure that the @link(TFunctionRecord.ResultType)
    is rdtDouble when assigning a @link(TRbwRealFunction).
    Failure to do so will
    lead to access violations or invalid results.
  }
  TRbwRealFunction = function(Values: array of pointer): double;
  {
    @Name is the prototype for functions in TRbwParser that
    return an integer.  To use a TRbwIntegerFunction, assign it to the
    IFunctionAddr of a TFunctionRecord, assign the remainder of the fields
    in the TFunctionRecord and then call the Add function of
    TRbwParser.Functions.  Be sure that the ResultType of the TFunctionRecord
    is rdtInteger when assigning a TRbwIntegerFunction.  Failure to do so will
    lead to access violations or invalid results.
  }
  TRbwIntegerFunction = function(Values: array of pointer): Integer;
  {
    @Name is the prototype for functions in TRbwParser that
    return a Boolean.  To use a TRbwBooleanFunction, assign it to the
    BFunctionAddr of a TFunctionRecord, assign the remainder of the fields
    in the TFunctionRecord and then call the Add function of
    TRbwParser.Functions.  Be sure that the ResultType of the TFunctionRecord
    is rdtBoolean when assigning a TRbwBooleanFunction.  Failure to do so will
    lead to access violations or invalid results.
  }
  TRbwBooleanFunction = function(Values: array of pointer): Boolean;
  {
    @Name is the prototype for functions in TRbwParser that
    return a string.  To use a TRbwStringFunction, assign it to the
    SFunctionAddr of a TFunctionRecord, assign the remainder of the fields
    in the TFunctionRecord and then call the Add function of
    TRbwParser.Functions.  Be sure that the ResultType of the TFunctionRecord
    is rdtString when assigning a TRbwStringFunction.  Failure to do so will
    lead to access violations or invalid results.
  }
  TRbwStringFunction = function(Values: array of pointer): string;

  {
    @Name is used to define the data type passed into or returned by
    a function.  In the case of rdtString used to define the data passed into
    a function, the data are passed into the function as a PString.
    Data are
    passed into a function as pointers to variables of the correct
    type. Results of the functions are values of the correct type rather
    than pointers.
  }
  TRbwDataType = (rdtDouble, rdtInteger, rdtBoolean, rdtString);

  {@name is defined for convenience here but is not used by @link(TRbwParser).}
  TRbwDataTypes = set of TRbwDataType;

  // @name is a pointer to a @link(TFunctionRecord).
  PFunctionRecord = ^TFunctionRecord;
{$WARNINGS OFF}
  {

    @abstract(A @Name is used to define a function that can be used in a
    TRbwParser.  To use the function, you must first assign the fields
    of the TFunctionRecord and then call TRbwParser.Functions.Add.)
    

    @longcode(#
  TFunctionRecord = record
    InputDataTypes: array of TRbwDataType;
    OptionalArguments: integer;
    CanConvertToConstant: boolean;
    Name: string;
    Prototype: string;
    Hidden: boolean;
    case ResultType: TRbwDataType of
      rdtDouble: (RFunctionAddr: TRbwRealFunction);
      rdtInteger: (IFunctionAddr: TRbwIntegerFunction);
      rdtBoolean: (BFunctionAddr: TRbwBooleanFunction);
      rdtString: (SFunctionAddr: TRbwStringFunction);
  end;
    #)
  }
  TFunctionRecord = record
    {
      @Name defines the type of data passed into the function
      assigned to RFunctionAddr, IFunctionAddr, BFunctionAddr, or
      SFunctionAddr.  The data are passed in as pointers to variables
      of the correct type.

      The length of @Name normally defines the maximum number of
      arguments that can be passed into the function.  However, if
      OptionalArguments is less than 0, an unlimited number of arguments can
      be passed to the function and the types of all those arguments must
      match the type of the last member of InputDataTypes;  In that case,
      the minimum number of required arguments is the length of InputDataTypes
      minus 1.

      See also : @link(TRbwDataType);
    }
    InputDataTypes: array of TRbwDataType;
    {
      If @Name is greater than 0, some of the arguments passed to
      the function may be nil pointers.  The nil pointers will always follow
      the non-nil pointers; no non-nil pointer may follow a nil pointer.
      The maximum number of nil pointers will be @Name.

      If @Name  is less than 0, the number of arguments passed to
      the function may be greater than the length of @link(InputDataTypes).
      The minimum number of arguments is the length of @link(InputDataTypes)
      minus 1.
      If more arguments are used, the types of those arguments must correspond
      to the value defined in the last member of @link(InputDataTypes) or to
      @link(OptionalType) if no @link(InputDataTypes) are defined.
    }
    OptionalArguments: integer;
    OptionalType: TRbwDataType;
    {
      @Name defines whether the result of a function may be
      considered a constant value if all of the values passed to the function
      in the values array are constants.

      Normally @Name should be set to True but if the function
      makes reference to global variables that may change between one
      evaluation of the expression and the next, @Name should
      be set to False.

      Pi is an example of a function for which CanConvertToConstant should
      be true.

      @Name is used when optimizing compiled expressions.

      Example:

      @LongCode(#
        var
          // ... others omitted in example.
          PiFunction : TFunctionRecord;
          SinFunction : TFunctionRecord;
      #)

         Pi is a constant so CanConvertToConstant is true.  Pi has no
         input variables so it will always be converted to a constant.
         When PiFunction is used to create a TFunctionClass,
         CanConvertToConstant will set the value of the
         TFunctionClass.AllowConversionToConstant property.
         That in turn will set the value of
         TExpression.AllowConversionToConstant.  The same happens with
         RFunctionAddr and InputDataTypes.

         _Pi is a function defined in the implementation

      @LongCode(#
        function _Pi(Values : array of pointer) : double;
        begin
          result := Pi;
        end;
      #)

      Sin will always return the same value if it's input is a constant
      so CanConvertToConstant is set true.  However, it will only be converted
      to a constant if all of it's input variables are constants.

      _Sin is a function defined in the implementation

      @LongCode(#
      function _Sin(Values : array of pointer) : double;
      begin
        result := Sin(PDouble(Values[0])^);
      end;
      #)

      @LongCode(#
      constructor TFunctionStringList.Create;
      begin
        inherited;
        CaseSensitive := False;
        Duplicates := dupError;
        Sorted := True;

        // ... others lines omitted in example.

        PiFunction.ResultType := rdtDouble;
        PiFunction.Name := 'Pi';
        SetLength(PiFunction.InputDataTypes, 0);
        PiFunction.OptionalArguments := 0;
        PiFunction.CanConvertToConstant := True;
        PiFunction.RFunctionAddr := _Pi;
        Add(PiFunction);

        // ... others lines omitted in example.

        PiFunction.ResultType := rdtDouble;
        PiFunction.Name := 'Pi';
        SetLength(PiFunction.InputDataTypes, 0);
        PiFunction.OptionalArguments := 0;
        PiFunction.CanConvertToConstant := True;
        PiFunction.RFunctionAddr := _Pi;
        Add(PiFunction);

        // ... others lines omitted in example.

      end;
      #)
    }
    CanConvertToConstant: boolean;
    {
      @Name is the unique identifier of the function.
    }
    Name: string;
    {
      @Name should give the names and arguments for a function as they
      should be shown on a GUI interface. The "|" character is used to
      indicate the position of the function in a hierarchy.
    }
    Prototype: string;
    {
      @Name determines whether or not the function should be
      visible in a GUI interface.
    }
    Hidden: boolean;
    { @name represents alternative valid spellings for the same function.
    }
    Synonyms: array of string;
    {
     @Name defines the type of data returned by the function.
     It must correspond to the value assigned to the RFunctionAddr,
     IFunctionAddr, BFunctionAddr, or SFunctionAddr.
    }
    case ResultType: TRbwDataType of
      {
        @Name is the address of the @Link(TRbwRealFunction) that is to be
        evaluated.
      }
      rdtDouble: (RFunctionAddr: TRbwRealFunction);
      {
        @Name is the address of the @Link(TRbwIntegerFunction) that is to be
        evaluated.
      }
      rdtInteger: (IFunctionAddr: TRbwIntegerFunction);
      {
        @Name is the address of the @Link(TRbwBooleanFunction) that is to be
        evaluated.
      }
      rdtBoolean: (BFunctionAddr: TRbwBooleanFunction);
      {
        @Name is the address of the @Link(TRbwStringFunction) that is to be
        evaluated.
      }
      rdtString: (SFunctionAddr: TRbwStringFunction);
  end;
{$WARNINGS ON}

  // @name is a pointer to a boolean.
  PBoolean = ^Boolean;

  {
    @abstract(@Name are used in the protected section of @Link(TExpression).)
    @longcode(#
  TVariables = record
    Datum: Pointer;
    DataType: TRbwDataType;
  end;
  #)
  }
  TVariables = record
    {
      @Name is a pointer to a value to be passed to a function.
    }
    Datum: Pointer;
    {
      @Name defines the type of data to which @Link(Datum) points.
    }
    DataType: TRbwDataType;
  end;

  {
    @abstract(@Name is raised if by @Link(TRbwParser) to report user errors
    and some programmer errors.)

    (Assertion failures represent programmer errors.)
  }
  ERbwParserError = class(Exception)
  public
    // @name: integer;
    ErrorType: integer;
    // sets @link(ErrorType) to ErrorMode.
    constructor CreateMode(const Msg: string; const ErrorMode: integer);
  end;

  {
    @abstract(@Name defines a function that can be called by @Link(TRbwParser).)
  }
  TFunctionClass = class(TObject)
  private
    //  @Name: TStrings;
    // See @link(Synonyms).
    FSynonyms: TStrings;
    // @Name: @link(TFunctionRecord);
    // @name stores the data represented by many of the public properties.
    FunctionRecord: TFunctionRecord;
    // @Name: string;
    // See @link(Prototype).
    FPrototype: string;
    // See @link(AllowConversionToConstant).
    function GetAllowConversionToConstant: boolean;
    // Gets value of BFunctionAddr property
    function GetBFunctionAddr: TrbwBooleanFunction;
    // See @link(Hidden).
    function GetHidden: boolean;
    // Gets value of IFunctionAddr property
    function GetIFunctionAddr: TrbwIntegerFunction;
    // See @link(InputDataCount).
    function GetInputDataCount: integer;
    // See @link(InputDataTypes).
    function GetInputDataTypes(const Index: integer): TRbwDataType;
    // See @link(Name).
    function GetName: string;
    // See @link(OptionalArguments).
    function GetOptionalArguments: integer; inline;
    // See @link(ResultType).
    function GetResultType: TRbwDataType;
    // Gets value of RFunctionAddr property
    function GetRFunctionAddr: TrbwRealFunction;
    // Gets value of SFunctionAddr property
    function GetSFunctionAddr: TrbwStringFunction;
    // See @link(AllowConversionToConstant).
    procedure SetAllowConversionToConstant(const Value: boolean); inline;
    // Sets value of BFunctionAddr property
    procedure SetBFunctionAddr(const Value: TrbwBooleanFunction);
    // See @link(Hidden).
    procedure SetHidden(const Value: boolean); inline;
    // Sets value of IFunctionAddr property
    procedure SetIFunctionAddr(const Value: TrbwIntegerFunction);
    // See @link(InputDataCount).
    procedure SetInputDataCount(const Value: integer);
    // See @link(InputDataTypes).
    procedure SetInputDataTypes(const Index: integer;
      const Value: TRbwDataType);
    // See @link(Name).
    procedure SetName(const Value: string);
    // See @link(OptionalArguments).
    procedure SetOptionalArguments(const Value: integer); inline;
    // Sets value of RFunctionAddr property
    procedure SetRFunctionAddr(const Value: TrbwRealFunction);
    // Sets value of SFunctionAddr property
    procedure SetSFunctionAddr(const Value: TrbwStringFunction);
    // See @link(Synonyms).
    procedure SetSynonyms(const Value: TStrings);
    function GetOptionalType: TRbwDataType;
    procedure SetOptionalType(const Value: TRbwDataType);
  public
    {
      @Name defines whether the result of a function may be
      considered a constant value if all of the values passed to the function
      in the values array are constants.

      Normally @Name should be set to True but if the
      function makes reference to global variables that may change between one
      evaluation of the expression and the next, @Name
      should be set to False.

      Pi is an example of a function for which @Name should
      be true.

      @Name is used when optimizing compiled expressions.
    }
    property AllowConversionToConstant: boolean
      read GetAllowConversionToConstant write SetAllowConversionToConstant;
    {
      @Name calls the inherited @Name and sets
      @Link(AllowConversionToConstant) to true.

      Users should generally not call @Name directly but instead create
      a @Link(TFunctionClass) by calling
      TFunctionStringList.@Link(TFunctionStringList.Add).
    }
    constructor Create;
    {@Name destroys the @Link(TFunctionClass).  Do not call @Name directly.
      Call Free instead.}
    destructor Destroy; override;
    {
      @Name is the address of a @Link(TRbwBooleanFunction) assigned to
      the @Link(TFunctionClass).  If @Link(ResultType) is of the wrong type,
      reading @Name will raise an exception. Writing @Name sets
      @Link(ResultType).
    }
    property BFunctionAddr: TRbwBooleanFunction read GetBFunctionAddr
      write SetBFunctionAddr;
    {
      @Name is the address of a @Link(TRbwIntegerFunction) assigned to
      the @Link(TFunctionClass).  If @Link(ResultType) is of the wrong type,
      reading @Name will raise an exception. Writing @Name sets
      @Link(ResultType).
    }
    property IFunctionAddr: TRbwIntegerFunction read GetIFunctionAddr
      write SetIFunctionAddr;
    {
      @Name normally defines the maximum number of
      arguments that can be passed into the function.  However, if
      @Link(OptionalArguments) is less than 0, an unlimited number of arguments
      can be passed to the function and the types of all those arguments must
      match the type of the last member of @Link(InputDataTypes).  In that case,
      the minimum number of required arguments is @Name
      minus 1.
    }
    property InputDataCount: integer read GetInputDataCount
      write SetInputDataCount;
    {
      @Name is used to define the data types passed into or returned
      by a function.  Data are
      passed into a function as pointers to variables of the correct
      type. Results of the functions are values of the correct type rather
      than pointers.
    }
    property InputDataTypes[const Index: integer]: TRbwDataType
      read GetInputDataTypes write SetInputDataTypes;
    property OptionalType: TRbwDataType
      read GetOptionalType write SetOptionalType;
    {
      @Name defines the name of the function.  The @Name of each function and
      variable in a @Link(TRbwParser) must be unique.
      seealso(Synonyms)
    }
    property Name: string read GetName write SetName;
    {
      @Name gives suggestions as to how the names and arguments
      for a function could be shown on a GUI interface.  The "|"
      character is used in the prototype to classify functions.  Portions
      of the prototype that appear before a "|" character is the
      classification to which the rest of the prototype belongs.
      Thus, the portions before the "|" could be used to populate a
      tree control.
    }
    property Prototype: string read FPrototype write FPrototype;
    {
      If @Name is greater than 0, up to that number of nil
      pointers may be passed to the function when it is evaluated.  All
      the nil pointers must follow all the non-nil pointers.

      If @Name is less than 0, an unlimited number of arguments
      may be passed to the function.  The data type of these pointers will
      correspond to the data type defined in the last member of
      @Link(InputDataTypes).
    }
    property OptionalArguments: integer read GetOptionalArguments
      write SetOptionalArguments;
    {
      @Name is used to define the data type passed returned by
      a function.  The results of the functions are values of the correct
      type rather than pointers.

      Example:

      Because OptionalArguments is less than 0, the CaseB, CaseI, CaseR, and
      CaseS functions can take an unlimited number of arguments.

      @longcode(#
      function _CaseBoolean(Values : array of pointer) : boolean;
      begin
        result := PBoolean(Values[PInteger(Values[0])^])^;
      end;

      function _CaseInteger(Values : array of pointer) : integer;
      begin
        result := PInteger(Values[PInteger(Values[0])^])^;
      end;

      function _CaseDouble(Values : array of pointer) : double;
      begin
        result := PDouble(Values[PInteger(Values[0])^])^;
      end;

      function _CaseString(Values : array of pointer) : String;
      begin
        result := PString(Values[PInteger(Values[0])^])^;
      end;

      var
        CaseBooleanFunction : TFunctionRecord;
        CaseIntegerFunction : TFunctionRecord;
        CaseDoubleFunction : TFunctionRecord;
        CaseStringFunction : TFunctionRecord;

      constructor TFunctionStringList.Create;
      begin
        inherited;
        CaseSensitive := False;
        Duplicates := dupError;
        Sorted := True;

        // ... others lines omitted in example.

        CaseBooleanFunction.ResultType := rdtBoolean;
        CaseBooleanFunction.Name := 'CaseB';
        SetLength(CaseBooleanFunction.InputDataTypes, 4);

        CaseBooleanFunction.InputDataTypes[0] := rdtInteger;
        CaseBooleanFunction.InputDataTypes[1] := rdtBoolean;
        CaseBooleanFunction.InputDataTypes[2] := rdtBoolean;
        CaseBooleanFunction.InputDataTypes[3] := rdtBoolean;
        CaseBooleanFunction.CanConvertToConstant := True;
        CaseBooleanFunction.OptionalArguments := -1;
        CaseBooleanFunction.BFunctionAddr := _CaseBoolean;
        Add(CaseBooleanFunction);

        CaseIntegerFunction.ResultType := rdtInteger;
        CaseIntegerFunction.Name := 'CaseI';
        SetLength(CaseIntegerFunction.InputDataTypes, 4);

        CaseIntegerFunction.InputDataTypes[0] := rdtInteger;
        CaseIntegerFunction.InputDataTypes[1] := rdtInteger;
        CaseIntegerFunction.InputDataTypes[2] := rdtInteger;
        CaseIntegerFunction.InputDataTypes[3] := rdtInteger;
        CaseIntegerFunction.OptionalArguments := -1;
        CaseIntegerFunction.CanConvertToConstant := True;
        CaseIntegerFunction.IFunctionAddr := _CaseInteger;
        Add(CaseIntegerFunction);

        CaseDoubleFunction.ResultType := rdtDouble;
        CaseDoubleFunction.Name := 'CaseR';
        SetLength(CaseDoubleFunction.InputDataTypes, 4);
        CaseDoubleFunction.InputDataTypes[0] := rdtInteger;
        CaseDoubleFunction.InputDataTypes[1] := rdtDouble;
        CaseDoubleFunction.InputDataTypes[2] := rdtDouble;
        CaseDoubleFunction.InputDataTypes[3] := rdtDouble;
        CaseDoubleFunction.OptionalArguments := -1;

        CaseDoubleFunction.CanConvertToConstant := True;
        CaseDoubleFunction.RFunctionAddr := _CaseDouble;
        Add(CaseDoubleFunction);

        CaseStringFunction.ResultType := rdtString;
        CaseStringFunction.Name := 'CaseS';
        SetLength(CaseStringFunction.InputDataTypes, 4);
        CaseStringFunction.InputDataTypes[0] := rdtInteger;
        CaseStringFunction.InputDataTypes[1] := rdtString;
        CaseStringFunction.InputDataTypes[2] := rdtString;
        CaseStringFunction.InputDataTypes[3] := rdtString;
        CaseStringFunction.OptionalArguments := -1;
        CaseStringFunction.CanConvertToConstant := True;
        CaseStringFunction.SFunctionAddr := _CaseString;
        Add(CaseStringFunction);
        // ... others lines omitted in example.
      end;

      #)
    }
    property ResultType: TRbwDataType read GetResultType;
    {
      @Name is the address of a @Link(TRbwRealFunction) assigned to the
      @Link(TFunctionClass).  If @Link(ResultType) is of the wrong type, reading
      @Name will raise an exception. Writing @Name sets
      @Link(ResultType).
    }
    property RFunctionAddr: TRbwRealFunction read GetRFunctionAddr
      write SetRFunctionAddr;
    {
      @Name is the address of a @Link(TRbwStringFunction) assigned to the
      @Link(TFunctionClass).  If @Link(ResultType) is of the wrong type, reading
      @Name will raise an exception. Writing @Name sets
      @Link(ResultType).
    }
    property SFunctionAddr: TRbwStringFunction read GetSFunctionAddr
      write SetSFunctionAddr;
    {
      @Name has no effect.  It is intended to be used to indicate whether or
      not the function specified by the @Link(TFunctionClass) should be visible
      to the user.
    }
    property Hidden: boolean read GetHidden write SetHidden;
    { @name holds a list of alternative acceptable spellings for a function.
      No alternate name should be the same as the name of any other function
      or one of its alternate names.
      @seealso(Name).}
    property Synonyms: TStrings read FSynonyms
      write SetSynonyms;
  end;

  {
    @abstract(@Name maintains a sorted list of @Link(TFunctionClass)es.)

    In the functions below, except where otherwise noted,
    angles are expressed in radians.

    The following functions are included:

    @unorderedList(
    @item(@bold(AbsI)(Value) returns the absolute value of Value.
    Value must be an integer.
    The value returned by AbsI will be an integer.)

    @item(@bold(AbsR)(Value) returns the absolute value of Value.
    Value can be either an integer or a real number.
    The value returned by AbsR will be a real number.)

    @item(@bold(ArcCos)(Value) returns the inverse cosine of Value.
    The return value is in the range from zero to Pi.)

    @item(@bold(ArcCosh)(Value) returns the inverse hyperbolic cosine of Value.)

    @item(@bold(ArcSin)(Value) returns the inverse sine of Value.
    The return value is in the range from -Pi/2 to +Pi/2.)

    @item(@bold(ArcSinh)(Value) returns the inverse hyperbolic sine of Value.)

    @item(@bold(ArcTan2)(Y, X) returns the inverse tangent of Y/X
    in the correct quadrant.
    The return value is in the range from -Pi to +Pi.)

    @item(@bold(ArcTanh)(Value) returns
    the inverse hyperbolic tangent of Value.)

    @item(@bold(CaseB)(Index, Boolean_Result1, Boolean_Result2, ...)
    is like @link(OverloadedFunctionList Case) except that it
    always returns a Boolean.)

    @item(@bold(CaseI)(Index, Integer_Result1, Integer_Result2, ...) 
    is like @link(OverloadedFunctionList Case) except that it
    always returns an integer.)

    @item(@bold(CaseR)(Index, Real_Result1, Real_Result2, ...) 
    is like @link(OverloadedFunctionList Case) except that it
    always returns a real number.)

    @item(@bold(CaseT)(Index, Text_Result1, Text_Result2, ...). 
    is like @link(OverloadedFunctionList Case) except that it
    always returns text. 
    CaseS is a synonym for CaseT.)

    @item(@bold(Copy)(Text_Value, StartIndex, Count) returns a substring
    of text_Value starting at the character indicated by
    StartIndex and extending for either Count characters or
    until the end text_Value is reached whichever is smaller.)

    @item(@bold(Cos)(Value) returns the cosine of Value.)

    @item(@bold(Cosh)(Value) returns the hyperbolic cosine of Value.)

    @item(@bold(DegToRad)(Value) converts Value from degrees to radians.
    See also RadToDeg.)

    @item(@bold(Distance)(X1, Y1, X2, Y2) calculates the distance between
    points (X1, Y1) and (X2, Y2).)

    @item(@bold(FactorialI)(Value_Less_than_13) returns the factorial of
    Value_Less_than_13 as an integer.)

    @item(@bold(FactorialR)(Value_Less_than_171) returns the factorial of
    Value_Less_than_171 as a real number.)

    @item(@bold(FloatToText)(Value) converts the real number Value to
    its string representation.
    FloatToStr is a synonym for FloatToText.)

    @item(@bold(Frac)(Value) returns the fractional part of Value.
    Value is a real number.)

    @item(@bold(IfB)(Boolean_Value, If_True_Boolean_Result,
    If_False_Boolean_Result) 
    is like @link(OverloadedFunctionList If) except that it
    always returns a Boolean.)

    @item(@bold(IfI)(Boolean_Value, If_True_Integer_Result,
    If_False_Integer_Result)
    is like @link(OverloadedFunctionList If) except that it
    always returns an integer.)

    @item(@bold(IfR)(Boolean_Value, If_True_Real_Result, If_False_Real_Result)
    is like @link(OverloadedFunctionList If) except that it
    always returns a real number.)

    @item(@bold(IfT)(Boolean_Value, If_True_Text_Result, If_False_Text_Result)
    is like @link(OverloadedFunctionList If) except that it
    always returns text. 
    IfS is a synonym for IfT.)

    @item(@bold(Interpolate)(Position, Value1, Distance1, Value2, Distance2).
    Interpolate returns

    (Position-Distance1)/(Distance2-Distance1)*(Value2-Value1)+Value1.

    As its name implies, this is an interpolation between Value1 and
    Value2 based on where Position is between Distance1 and Distance2.
    See also @link(OverloadedFunctionList MultiInterpolate).)

    @item(@bold(IntPower)(Base, Exponent) returns Base raised to the Exponent
    power.  Base must be a real number or integer.
    Exponent must be an integer. IntPower returns a real number.
    See also Power.)

    @item(@bold(IntToText)(Value) converts the integer number Value
    to its string representation.
    IntToStr is a synonym for IntToText.)

    @item(@bold(Length)(Text_Value) returns the number of
    characters in text_Value.)

    @item(@bold(ln)(Value) returns the natural log of Value.)

    @item(@bold(log10)(Value) returns the log to the base 10 of Value.)

    @item(@bold(logN)(BaseN, Value) returns the
    log to the base BaseN of Value.)

    @item(@bold(LowerCase)(Text_Value) returns text_Value with all its
    characters converted to lower case.  See also: UpperCase.)

    @item(@bold(MaxI)(Integer_Value1, Integer_Value2, ...) returns whichever of its
    arguments is the largest.  Its arguments must be integers.
    The result will be an integer.)

    @item(@bold(MaxR)(Real_Value1, Real_Value2, ...) returns whichever of its
    arguments is the largest.
    Its arguments must be either integers or real numbers.
    The result will be a real number.)

    @item(@bold(MinI)(Integer_Value1, Integer_Value2, ...) returns whichever
    of its arguments is the smallest.
    Its arguments must be integers.  The result will be an integer.)

    @item(@bold(MinR)(Real_Value1, Real_Value2, ...) returns whichever of its
    arguments is the smallest.
    Its arguments must be either integers or real numbers.
    The result will be a real number.)

    @item(@bold(Odd)(Value) returns True if Value is an odd number.
    Otherwise it returns False.  Value must be an integer.)

    @item(@bold(Pi) returns the ratio of the
    circumference of a circle to its diameter.)

    @item(@bold(Pos)(SubString, StringValue) returns the position of the first
    instance of SubString within StringValue.
    If Substring does not occur within StringValue, Pos returns 0.)

    @item(@bold(PosEx)(SubText, Text_Value, Offset) returns the
    position of the first
    instance of SubText within Text_Value that starts on or after Offset.
    If SubText does not occur within Text_Value, on or after Offset,
    PosEx returns 0.  If Offset equals one, PosEx is equivalent to Pos.)

    @item(@bold(Power)(Base, Exponent) returns Base raised to the
    Exponent power.
    Base and Exponent must be real numbers or integers. Power returns
    a real number.
    See also IntPower.)

    @item(@bold(RadToDeg)(Value) converts Value from radians to degrees.
    See also DegToRad.)

    @item(@bold(Round)(Value) converts Value to the nearest integer.
    In the case of a number that is exactly halfway between two
    integers, it converts it to whichever one is even.
    See also Trunc.)

    @item(@bold(Sin)(Value) returns the sine of Value.)

    @item(@bold(Sinh)(Value) returns the hyperbolic sine of Value.)

    @item(@bold(SqrI)(Integer_Value) returns integer_Value squared. Integer_Value
    must be an integer.  The result of SqrI will be an integer.)

    @item(@bold(SqrR)(Real_Value) returns real_Value squared. Real_Value
    can be either an integer or a real number.
    The result of SqrR will be a real number.)

    @item(@bold(Sqrt)(Value) returns the square root of Value.)

    @item(@bold(TextToFloat)(Text_Value) converts text_Value to a real number.
    If text_Value can not be converted, TextToFloat causes an error.
    StrToFloat is a synonym for TextToFloat.)

    @item(@bold(TextToFloatDef)(Text_Value, DefaultResult) converts
    text_Value to a real number.
    If text_Value can not be converted, DefaultResult
    is returned instead.
    StrToFloatDef is a synonym for TextToFloatDef.)

    @item(@bold(TextToInt)(Text_Value) converts text_Value to an integer.
    If text_Value can not be converted, TextToInt causes an error.
    StrToInt is a synonym for TextToInt.)

    @item(@bold(TextToIntDef)(Text_Value, DefaultResult) converts text_Value
    to an integer.  If text_Value can not be converted,
    DefaultResult is returned instead.
    StrToIntDef is a synonym for TextToIntDef.)

    @item(@bold(Tan)(Value) returns the tangent of Value.)

    @item(@bold(Tanh)(Value) returns the hyperbolic tangent of Value.)

    @item(@bold(Trim)(Text_Value) removes leading and trailing
    spaces and control characters from Value.)

    @item(@bold(Trunc)(Value) truncates Value to an integer by rounding it towards zero.
    See also Round.)

    @item(@bold(UpperCase)(Text_Value) returns
    text_Value with all its characters
    converted to upper case.  See also LowerCase.)
    )

  }
  TFunctionStringList = class(TStringList)
  private
    // See @link(FunctionClass).
    function GetFunctionClass(const Index: integer): TFunctionClass;
  public
    {
      @Name creates a @Link(TFunctionClass) based on FunctionRecord, calls the
      TStringList.AddObject method using the function name and the
      @Link(TFunctionClass) and returns the @Link(TFunctionClass) that it
      created.  Any @Link(TFunctionClass) created in this way is owned by the
      TFunctionStringList.
    }
    function Add(const FunctionRecord: TFunctionRecord): Integer; reintroduce;
    {
      @Name sets the number of items in the TFunctionStringList to 0 and Free's
      all the TFunctionClass it created.
    }
    procedure Clear; override;
    {
      @Name creates a TFunctionStringList and also creates a variety of
      @Link(TFunctionClass)'es.

      Users should not generally call @Name directly but instead use the
      @Link(TFunctionStringList) already created in
      TRbwParser.@Link(TRbwParser.Functions).
    }
    constructor Create;
    {
      @Name destroys the @Link(TFunctionClass) at the position Index.
    }
    procedure Delete(Index: Integer); override;
    {
      @Name destroys the @Link(TFunctionStringList).
      Do not call @Name directly. Call Free instead.
    }
    destructor Destroy; override;
    {
      @Name returns the @Link(TFunctionClass) at position index.
    }
    property FunctionClass[const Index: integer]: TFunctionClass
      read GetFunctionClass; default;
  end;

  {
    @abstract(@name defines storage for a constant value such as "Abc",
    1, 2.5, or @True.)
  }
  TConstant = class(TObject)
  private
    // @Name: string;
    // See @Link(ResultString).
    // Ordinarily access to FResultString should be avoided.
    FResultString: string;
    // See @Link(ResultString).
    procedure SetResultString(const Value: string);
  protected
    // @name: Pointer;
    // @name points to the address of the result.
    FResult: Pointer;
    // @name: @link(TRbwDataType);
    // @name specifies what type of data is stored in FResult.
    FResultType: TRbwDataType;
    // Create a TConstant and specify @link(FResultType).
    constructor Create(const DataType: TRbwDataType); overload;
    {@name adds a new line to List that shows its position in a hierarchy.
     The new line begins with Level
     number of tab characters.  This is followed by @link(Decompile) followed by
     another tab character and then @link(Decompile) again.
     }
    procedure MakeDiagram(List: TStringList; Level: integer); virtual;
    // Convert the result to a string.
    function ValueToString: string;
    // if @link(FResultType) is rdtString, ResultString is the string that is stored.
    property ResultString: string read FResultString write SetResultString;
  public
    {
      @Name creates a @classname and sets @Link(ResultType).
    }
    constructor Create(Value: string); overload;
    {
      @Name creates a @classname and sets @Link(ResultType).
    }
    constructor Create(const Value: Boolean); overload;
    {
      @Name creates a @classname and sets @Link(ResultType).
    }
    constructor Create(const Value: integer); overload;
    {
      @Name creates a @classname and sets @Link(ResultType).
    }
    constructor Create(const Value: double); overload;
    {
      @Name destroys the @Link(TConstant).  Do not call @Name directly.
      Call Free instead.
    }
    destructor Destroy; override;
    {
      @Name defines the type of data stored in the @Link(TConstant).
    }
    property ResultType: TRbwDataType read FResultType;
    {
      @Name returns the value stored in @classname
      if that value is a string. Other wise it generates an error.
    }
    function StringResult: string;
    {
      @Name returns the value stored in @classname
      if that value is a boolean. Other wise it generates an error.
    }
    function BooleanResult: boolean;
    {
      @Name returns the value stored in @classname
      if that value is a integer. Other wise it generates an error.
    }
    function IntegerResult: Integer;
    {
      @Name returns the value stored in @classname
      if that value is a integer or double. Other wise it generates an error.
    }
    function DoubleResult: double;
    {
      @Name converts the value stored in the @classname to a
      string that can be compiled into an equivalent @classname.
    }
    function Decompile: string; virtual;
    function DecompileDisplay: string; virtual;
  end;

  {
    @abstract(@Name is the abstract ancestor of @Link(TCustomVariable)
    and @Link(TExpression).
    It adds @Link(TCustomValue.Name) and
    @Link(TCustomValue.Classification) properties to @Link(TConstant).)
  }
  TCustomValue = class(TConstant)
  private
    // Always upper case
    FName: string;
    // FUserName is the mixed-case version of @link(FName).
    // It is used in @link(Decompile).
    FUserName: string;
    FClassification: string;
  public
    {@name is meant to be used as a hint to a GUI on how to organize a
    series of instances of @classname.  It is not used internally.}
    property Classification: string read FClassification write FClassification;
    {
      @Name defines how the @Link(TCustomValue) should be identified.
      It is set via Create.
    }
    property Name: string read FName;
    {
      @Name calls the inherited constructor and sets @Link(Name).
    }
    constructor Create(const VariableName: string; const DataType:
      TRbwDataType);
    {
      @Name returns the @Link(Name) of the @Link(TCustomValue).
    }
    function Decompile: string; override;
    function DecompileDisplay: string; override;
  end;

  {
    @abstract(@Name is the abstract ancestor of @Link(TRealVariable),
    @Link(TIntegerVariable), @Link(TBooleanVariable)
    and @Link(TStringVariable).)
    It validates the @Link(TCustomValue.Name)
    in @Link(TCustomVariable.Create).
  }
  TCustomVariable = class(TCustomValue)
  private
    FDisplayName: string;
    function GetDisplayName: string;
    procedure SetDisplayName(const Value: string);
  public
    {
      @Name calls inherited create and then validates VariableName.
      Variable @Link(TCustomValue.Name)s must start with a letter or underscore.
      The rest of the name must consist of letters underscores and digits.
    }
    constructor Create(const VariableName: string; const DataType:
      TRbwDataType; const NameToDisplay: string);
    property DisplayName: string read GetDisplayName write SetDisplayName;
    function Decompile: string; override;
    function DecompileDisplay: string; override;
  end;

  {
    @abstract(@Name stores a double that may change from one execution of
    a @Link(TExpression) to the next.)
  }
  TRealVariable = class(TCustomVariable)
  private
    // See @link(Value).
    function GetValue: double;
    // See @link(Value).
    procedure SetValue(const Value: double);
  public
    {
      @Name calls the inherited create.
    }
    constructor Create(const VariableName: string; const NameToDisplay: string);
    {
      @Name is the double stored by @Link(TRealVariable).
    }
    property Value: double read GetValue write SetValue;
  end;

  TRealVariableClass = class of TRealVariable;

  {
    @abstract(@Name stores a integer that may change from one execution of
    a @Link(TExpression) to the next.)
  }
  TIntegerVariable = class(TCustomVariable)
  private
    // See @link(Value).
    function GetValue: Integer;
    // See @link(Value).
    procedure SetValue(const Value: Integer);
  public
    {
      @Name calls the inherited create.
    }
    constructor Create(const VariableName: string; const NameToDisplay: string);
    {
      @Name is the Integer stored by @Link(TIntegerVariable).
    }
    property Value: Integer read GetValue write SetValue;
  end;

  TIntegerVariableClass = class of TIntegerVariable;

  {
    @abstract(@Name stores a boolean that may change from one execution of
    a @Link(TExpression) to the next.)
  }
  TBooleanVariable = class(TCustomVariable)
  private
    // See @link(Value).
    function GetValue: Boolean;
    // See @link(Value).
    procedure SetValue(const Value: Boolean);
  public
    {
      @Name calls the inherited create.
    }
    constructor Create(const VariableName: string; const NameToDisplay: string);
    {
      @Name is the Boolean stored by @Link(TBooleanVariable).
    }
    property Value: Boolean read GetValue write SetValue;
  end;

  TBooleanVariableClass = class of TBooleanVariable;

  {
    @abstract(@Name stores a string that may change from one execution of
    a @Link(TExpression) to the next.)
  }
  TStringVariable = class(TCustomVariable)
  private
    // See @link(Value).
    function GetValue: string;
    // See @link(Value).
    procedure SetValue(const Value: string);
  public
    {
      @Name calls the inherited create.
    }
    constructor Create(const VariableName: string; const NameToDisplay: string);
    {
      @Name is the String stored by @Link(TStringVariable).
    }
    property Value: string read GetValue write SetValue;
  end;

  TStringVariableClass = class of TStringVariable;

  TSpecialImplementorList = class;

  TExpression = class;

  TNotifierComponent = class(TComponent)
  private
    FExpression: TExpression;
  public
    property Expression: TExpression read FExpression;
    constructor Create(AnOwner: TExpression); reintroduce;
  end;

  TDecompileType = (dtInternal, dtDisplay);

  {
    @abstract(@Name is the compiled version of an Expression
    generated by the
    TRbwParser.@Link(TRbwParser.Compile) method.)  It can be evaluated by using
    the Evaluate method and the result can then be read
    using the @Link(TConstant.BooleanResult) method,
    @Link(TConstant.DoubleResult) method,
    @Link(TConstant.IntegerResult) method, or
    @Link(TConstant.StringResult) method.
    The correct one to read can be determined from the
    @Link(TConstant.ResultType) property.

    Every instance of @Name is owned by the instance of @Link(TRbwParser)
    that compiled it.
  }
  TExpression = class(TCustomValue)
  private
    FSpecialImplementorList: TSpecialImplementorList;
    // @name is the number of optional arguments in the function that will
    // be evaluated.
    FOptionalArguments: integer;
    // @name is the address of the function used to evaluate the
    // TExpression.
    FunctionAddr: Pointer;
    // @name is the argument passed to the function used
    // to evaluate the @classname.
    VariablesForFunction: array of Pointer;
    // See @link(AllowConversionToConstant).
    FAllowConversionToConstant: boolean;
    // See @link(Tag)
    FTag: integer;
    // StringVariableIndicies indicates which members of
    // @link(VariablesForFunction) refer to strings.
    StringVariableIndicies: array of integer;
    // StringVariableCount is the number of strings in
    // @link(VariablesForFunction).
    StringVariableCount: integer;
    // FVariablesUsed is used to hold the result of @link(VariablesUsed).
    FVariablesUsed: TStringList;
    // FTopLevel is used in @link(Decompile) to determine whether or not to include
    // parenthesis around the outermost item.
    FTopLevel: boolean;
    // @name is the prototype of the base @classname without any hints about
    // where it belongs in a hierarchy.
    // @seealso(TFunctionRecord.Prototype)
    // @name is used in @link(MakeDiagram).
    FPrototype: string;
    // @name is used to provide notification to components that
    // used an instance of @name when the instance is destroyed.
    FNotifier: TNotifierComponent;
    // Create a TExpression.
    constructor Create(const VariableName: string; const DataType: TRbwDataType;
      const CanConvertToConstant: boolean; SpecialImplementorList: TSpecialImplementorList); overload;
    // Create a TExpression.
    constructor Create(const FunctionRecord: TFunctionRecord; SpecialImplementorList: TSpecialImplementorList); overload;
    // Create a TExpression.
    constructor Create(const VariableName: string; const DataType:
      TRbwDataType; SpecialImplementorList: TSpecialImplementorList);
      overload;
    // Initializes certain variables.  Called by Create.
    procedure Initalize(const FunctionAddress: Pointer;
      const DataTypes: array of TRbwDataType; const OptionalArguments: integer);
    // See @link(Variables).
    function GetVariables(const Index: integer): TConstant;
    // See @link(Variables).
    procedure SetVariables(const Index: integer; const Value: TConstant);
    // if the expression can be represented as a constant value,
    // ConvertToConstant returns a @link(TConstant) that represents
    // that value.  Otherwise, it returns nil.
    function ConvertToConstant: TConstant;
    // If optional arguments are used, ResetDataLength is used to
    // resets the length of arrays to the correct length.
    procedure ResetDataLength(const Count: integer);
    // See @link(AllowConversionToConstant).
    procedure SetAllowConversionToConstant(const Value: boolean); inline;
    function DecompileByType(DecompileType: TDecompileType): string;
    {
      @Name defines whether the result of a function may be
      considered a constant value if all of the values passed to the function
      in the values array are constants.

      Normally @Name should be set to True but if the
      function makes reference to global variables that may change between one
      evaluation of the expression and the next, @Name
      should be set to False.

      Pi is an example of a function for which @Name should
      be true.

      @Name is used when optimizing compiled expressions.
    }
    property AllowConversionToConstant: boolean read FAllowConversionToConstant
      write SetAllowConversionToConstant;
    {
      @Name define the data passed to the function when @Link(Evaluate)
      is called.
      Any descendant of @Link(TConstant) may be assigned to @Name.
    }
    property Variables[const Index: integer]: TConstant read GetVariables
      write SetVariables;
    // @name initializes @link(VariablesForFunction) and @link(StringVariableIndicies)
    procedure FillVariables;
    procedure SetResultFromFunction;
  protected
     {
      @Name is set to false if a @Link(TExpression) is equivalent to a
      @Link(TConstant). In such cases, the @Link(TExpression)
      is evaluated when it
      is created and does not need to be reevaluated later.  An example would
      be if the expression used to create the @Link(TExpression) was "1 + 1".
      This would be converted to "2".
    }
    ShouldEvaluate: boolean;
    {
      @name: array of @link(TVariables);
      @Name holds the arguments used to evaluate the @Link(TExpression).
    }
    Data: array of TVariables;
    // See @link(VariablesUsed).
    function GetVariablesUsed: TStringList; virtual;
    {@name fills List with a diagram of itself. First, it
     adds a new line to List.  The new line begins with Level
     number of tab characters.  This is followed by its own name followed by
     another tab character and then @link(Decompile). Finally it calls
     @name for each of its children passing on List and Level+1.
     }
    procedure MakeDiagram(List: TStringList; Level: integer); override;
  public
    {
      @Name creates a @Link(TExpression) based on a @Link(TFunctionClass).
      The values of any variables used by the @Link(TExpression)
      must also be set before calling @Link(Evaluate).
    }
    constructor Create(const FunctionClass: TFunctionClass;
      SpecialImplementorList: TSpecialImplementorList); overload; virtual;
    {
      @Name converts the value stored in the @Link(TExpression)
      to a string that can be compiled into an equivalent @Link(TExpression)
      together with its arguments.
    }
    function Decompile: string; override;
    function DecompileDisplay: string; override;
    {
      @Name destroys the @Link(TExpression).  Do not call Destroy directly.
      Call Free instead.
    }
    destructor Destroy; override;
    {
      @Name evaluates the expression and sets the result which may then be
      read using @Link(TConstant.BooleanResult),
      @Link(TConstant.DoubleResult),
      @Link(TConstant.IntegerResult), or
      @Link(TConstant.StringResult)
      depending on the @Link(TConstant.ResultType).

      Example:

      @Longcode(#
      procedure TForm1.Button1Click(Sender: TObject);
      begin
        RbwParser1.Compile(Edit1.Text);
        RbwParser1.CurrentExpression.Evaluate;
        case RbwParser1.CurrentExpression.ResultType of
          rdtDouble:
            begin
              Label1.Caption := 'result: '
                + FloatToStr(RbwParser1.CurrentExpression.DoubleResult);
            end;
          rdtInteger:
            begin
              Label1.Caption := 'result: '
                + IntToStr(RbwParser1.CurrentExpression.IntegerResult);
            end;
          rdtBoolean:
            begin
              if RbwParser1.CurrentExpression.BooleanResult then
              begin
                Label1.Caption := 'result: ' + 'True';
              end
              else
              begin
                Label1.Caption := 'result: ' + 'False';
              end;
            end;
          rdtString:
            begin
              Label1.Caption := 'result: '
                + RbwParser1.CurrentExpression.StringResult;
            end;
        else Assert(False);
        end;
      end;
      #)
    }
    procedure Evaluate; virtual;
    {
      @Name usually calls Create to create a @Link(TExpression) based
      on FunctionClass. However, in some cases, it creates a descendant
      of @Link(TExpression).  Many of the descendents are declared
      in the implementation section of
      RBW_Parser.pas.  Others can be generated via
      @Link(TRbwParser.SpecialImplementorList
      TRbwParser.SpecialImplementorList).
      One such descendant is @Link(TSelectExpression) which overrides
      @Link(Evaluate).
    }
    class function New(const FunctionClass: TFunctionClass;
      SpecialImplementorList: TSpecialImplementorList): TExpression;
      virtual;
    {
      @Name has no predefined meaning. The Tag property is provided for the
      convenience of developers. It can be used for storing an
      additional integer value or it can be typecast to any 32-bit value
      such as a component reference or a pointer.
    }
    property Tag: integer read FTag write FTag;
    {
      @Name returns True if Variable is used by the @Link(TExpression).
    }
    function UsesVariable(const Variable: TCustomVariable): boolean; virtual;
    {
      @Name is a list of all the variables used by the
      @Link(TExpression).
      The Objects properties holds the variables themselves.
      The TStringList is owned by the @classname.

      @name is sorted, ignores duplicates,
      and is not case-sensitive;
    }
    property VariablesUsed: TStringList read GetVariablesUsed;
    { @name fills List with a diagram of itself.  Each line begins with
      N tab characters. The number of tab characters indicates the position
      of that line in a hierarchy of child @link(TCustomValue)s.
      Next the line has the name of the @link(TCustomValue), another tab
      character and then a @link(Decompile)d version of the @link(TCustomValue).
    }
    procedure Diagram(List: TStringList);
    // @name is used to provide notification to components that
    // used an instance of @name when the instance is destroyed.
    property Notifier: TNotifierComponent read FNotifier;
    function UsesFunction(FunctionName: string): boolean;
  end;

  // @name is used in @link(TSpecialImplementor) to create a descendant
  // of @link(TExpression).
  TExpressionClass = class of TExpression;

  {
    @abstract(@Name is used in conjunction with
    @Link(TRbwParser.SpecialImplementorList
    TRbwParser.SpecialImplementorList)
    to create a descendant of @Link(TExpression)
    in TExpression.@Link(TExpression.New).)
  }
  TSpecialImplementor = class
  public
    // @Name: @link(TFunctionClass);
    // @Name defines the interface of the expression.
    FunctionClass: TFunctionClass;
    // @name: @link(TExpressionClass);
    // @Name is the class of TExpression that will be created.
    Implementor: TExpressionClass;
  end;

  {
    @abstract(@Name is the type of
    @Link(TRbwParser.SpecialImplementorList
    TRbwParser.SpecialImplementorList).  It is used together with
    @Link(TSpecialImplementor) to create a descendant of @Link(TExpression)
    in TExpression.@Link(TExpression.New).)
  }
  TSpecialImplementorList = class(TObject)
  private
    // @name: TList;
    // is used to store @link(TSpecialImplementor)s.
    FList: TList;
    // See @link(Count).
    function GetCount: integer;
    // See @link(Items).
    function GetItems(const Index: integer): TSpecialImplementor;
    // See @link(Items).
    procedure SetItems(const Index: integer;
      const Value: TSpecialImplementor);
    // See @link(Capacity).
    function GetCapacity: integer;
    // See @link(Capacity).
    procedure SetCapacity(const Value: integer);
  public
    {
      @Name creates an instance of @link(TSpecialImplementorList).
    }
    constructor Create;
    {
      @Name destroys a @link(TSpecialImplementorList). Do not call @Name.
      Call Free instead.
    }
    destructor Destroy; override;
    {
      @Name is the number of @Link(TSpecialImplementor)s in the
      @link(TSpecialImplementorList).
    }
    property Count: integer read GetCount;
    {
      @Name is the number of @Link(TSpecialImplementor)s that the
      @link(TSpecialImplementorList) can hold without reallocating memory.

      Setting @Name to a value high enough to hold all the
      @Link(TSpecialImplementor)s that it will hold before @Link(Add)ing
      them, can speed up @Link(Add)ing the @Link(TSpecialImplementor)s.
    }
    property Capacity: integer read GetCapacity write SetCapacity;
    {
      @Name provides access to the @Link(TSpecialImplementor)s held by the
      @Link(TSpecialImplementorList).
    }
    property Items[const Index: integer]: TSpecialImplementor read GetItems write
      SetItems; default;
    {
      @Name adds a TSpecialImplementor to @Link(Items).  The items that are
      added are not owned by the @Link(TSpecialImplementorList).
    }
    function Add(const Item: TSpecialImplementor): Integer;
    {
      @Name  removes everything from @Link(Items) and sets the @Link(Count)
      and @Link(Capacity) to 0.
    }
    procedure Clear;
    {
      @Name removes the @Link(TSpecialImplementor) at position Index from
      @Link(Items);
    }
    procedure Delete(const Index: Integer);
    {
      @Name removes the @Link(TSpecialImplementor) indicated by Item
      from @Link(Items) and returns its former position.  If it was not
      in @Link(Items) it returns -1.
    }
    function Remove(const Item: TSpecialImplementor): Integer;
  end;

  // @name is used in @link(TOperatorDefinition) to indicate how many
  // arguments an operator has.
  TArgumentCount = (acOne, acTwo);
  // @name is used in @link(TOperatorDefinition) to whether the new expression
  // should be created with @link(TExpression.Create TOperator.Create)
  // or @link(TExpression.New TOperator.New).
  TCreationMethod = (cmCreate, cmNew);

  // @name is used to define operators that are between two arguments
  // or are before a single argument.
  TOperator = class(TExpression)
  private
    function DecompileByType(DecompileType: TDecompileType): string;
  public
    {
      Decompile converts the value stored in the TOperator to a
      string that can be compiled into an equivalent TOperator
      together with its argument.
    }
    function Decompile: string; override;
    function DecompileDisplay: string; override;
  end;

  // @name is used in @link(TOperatorArgumentDefinition) to indicate
  // what type of operator should be created.
  TOperatorClass = class of TOperator;

  // @name is used to define valid argument types for an operator.
  // and to store data required to create an instance of a @link(TOperator).
  TOperatorArgumentDefinition = class(TObject)
    // @name is the first argument of the operator.  For operators with
    // one argument, the argument comes after the operator. For operators with
    // two argument, the argument comes before the operator.
    FirstArgumentType: TRbwDataType;
    // @name is the second argument of the operator.  For operators with
    // one argument, @name is not used. For operators with
    // two argument, the argument comes after the operator.
    SecondArgumentType: TRbwDataType;
    // @name defines the type of @link(TOperator) that will be created.
    OperatorClass: TOperatorClass;
    // @name helps define the TOperator that will be created.
    // @name is not owned by the @classname so it must be created and
    // freed externally.
    FunctionClass: TFunctionClass;
    // See @link(TCreationMethod).
    CreationMethod: TCreationMethod;
  end;

  // @name is used to determine in which order operators will be evaluated.
  // p1 will be evaluated first; p5 will be evaluated last.
  TPrecedence = (p1, p2, p3, p4, p5);

  // Name is used to restrict ensure that in @link(TOperatorDefinition)
  // only @link(TOperatorArgumentDefinition)s are added to
  // @link(TOperatorDefinition.ArgumentDefinitions).
  // @name takes ownership of all @link(TOperatorArgumentDefinition)s
  // added to it.
  TArgumentList = class(TObject)
  private
    // @name is an internal list of @link(TOperatorArgumentDefinition)s.
    // @name is actually a TObjectList.
    FList: TList;
    // See @link(Count).
    function GetCount: integer;
    // See @link(Items).
    function GetItems(Index: integer): TOperatorArgumentDefinition;
  public
    // @name adds a @link(TOperatorArgumentDefinition) to @classname.
    procedure Add(Item: TOperatorArgumentDefinition);
    // @name provides access to the @link(TOperatorArgumentDefinition)
    // in @classname.
    property Items[Index: integer]: TOperatorArgumentDefinition
      read GetItems; default;
    // @name is the number of @link(TOperatorArgumentDefinition)s in
    // @classname.
    property Count: integer read GetCount;
    // @name creates an instance of @classname.
    constructor Create;
    // @name destroys the current instance of @classname along with
    // and @link(TOperatorArgumentDefinition)s it contains.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  // @name defines an operator.
  TOperatorDefinition = class(TObject)
  private
    // See @link(OperatorName).
    FOperatorName: AnsiString;
    // See @link(Precedence).
    FPrecedence: TPrecedence;
    // See @link(ArgumentCount).
    FArgumentCount: TArgumentCount;
    // See @link(ArgumentDefinitions).
    FArgumentDefinitions: TArgumentList;
    // See @link(SignOperator).
    FSignOperator: boolean;
  public
    // @name is the name of the operator.
    property OperatorName: AnsiString read FOperatorName write FOperatorName;
    // @name indicates how many arguments the operator has.
    property ArgumentCount: TArgumentCount read FArgumentCount write FArgumentCount;
    // See @link(TPrecedence)
    property Precedence: TPrecedence read FPrecedence write FPrecedence;
    // @name must contain one or more @link(TOperatorArgumentDefinition)s.
    property ArgumentDefinitions: TArgumentList read FArgumentDefinitions;
    // @name indicates whether or not the operator is a sign operator.
    property SignOperator: boolean read FSignOperator write FSignOperator;
    // @name creates an instance of @classname.
    Constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name. Call Free instead.
    destructor Destroy; override;
  end;

  {
    @abstract(@Name compiles expression in strings
    via the @Link(TRbwParser.Compile)
    method into @Link(TExpression) objects that it owns.)  It can also create
    @Link(TRealVariable)s, @Link(TIntegerVariable)s, @Link(TBooleanVariable)s,
    and @Link(TStringVariable)s that can be used in the @Link(TExpression)
    via the @Link(TRbwParser.CreateVariable) method. Alternately, such variables
    created by another instance of TRbwParser can be used via the
    @Link(TRbwParser.RegisterVariable) method.

    All variables and expressions created by an instance of
    @Name are owned by it.

    If several TRbwParsers are used and variables owned by one are used in
    another one, @Link(TRbwParser.ClearExpressions) and
    @Link(TRbwParser.ClearVariables) should
    be called in all of them before any of them is destroyed.

    See @link(TFunctionStringList) and @link(OverloadedFunctionList)
    for a list of the functions included by default.
  }
  TRbwParser = class(TComponent)
  private
    // @name contains the names of the single character operators.
    Operators: TStringList;
    // name contains the names of the operators that have
    // two or more characters in their names.
    WordOperators: TStringList;
    // @name contains the names of the sign operators.
    Signs: set of AnsiChar;
    // See @link(TSpecialImplementorList) and @link(SpecialImplementorList).
    FSpecialImplementorList: TSpecialImplementorList;
    // @name: @link(TExpression);
    // See @link(CurrentExpression).
    FCurrentExpression: TExpression;
    // @name: TStringList;
    // See @link(Expressions).
    FExpressions: TStringList;
    // @name: @link(TFunctionStringList);
    // See @link(Functions).
    FFunctions: TFunctionStringList;
    // @name: TStringList;
    // See @link(Variables).
    FVariables: TStringList;
    // @name: TObjectList;
    // @name holds all the @link(TCustomVariable)s owned by the
    // @link(TRbwParser).
    FOwnedVariables: TObjectList;
    // @name contains a series of @link(TOperatorDefinition)s.
    FOpereratorDefinitions: TList;
    FCachedFindResult: integer;
    // See @link(Expressions).
    function GetExpressions(const Index: integer): TExpression;
    // See @link(Variables).
    function GetVariable(const Index: integer): TCustomValue;
    // @name defines the "not" operator.
    procedure DefineNotOperator;
    // @name defines the "+" sign operator.
    procedure DefinePlusSignOperator;
    // @name defines the "-" sign operator.
    procedure DefineMinusSignOperator;
    // @name defines the "*" operator.
    procedure DefineTimesOperator;
    // @name defines the "/" operator.
    procedure DefineDivideOperator;
    // @name defines the "div" operator.
    procedure DefineDivOperator;
    // @name defines the "mod" operator.
    procedure DefineModOperator;
    // @name defines the "and" operator.
    procedure DefineAndOperator;
    // @name defines the "+" (addition or concatenation) operator.
    procedure DefinePlusOperator;
    // @name defines the "-" (subtraction) operator.
    procedure DefineMinusOperator;
    // @name defines the "or" operator.
    procedure DefineOrOperator;
    // @name defines the "xor" operator.
    procedure DefineXorOperator;
    // @name defines the "=" operator.
    procedure DefineEqualsOperator;
    // @name defines the "<>" operator.
    procedure DefineNotEqualsOperator;
    // @name defines the "<" operator.
    procedure DefineLessThanOperator;
    // @name defines the ">" operator.
    procedure DefineGreaterThanOperator;
    // @name defines the "<=" operator.
    procedure DefineLessThanOrEqualsOperator;
    // @name defines the ">=" operator.
    procedure DefineGreaterThanOrEqualsOperator;
    // @name defines the "^" operator
    procedure DefinePowerOperator;
    procedure DefinePowerOperator2;
    { Private declarations }
  protected
    { Protected declarations }
  public
    // @name adds an operator to the operators recognized by @Classname.
    // The @classname will take ownership of the @link(TOperatorDefinition)
    // but not of any @link(TOperatorArgumentDefinition) that it has.
    procedure AddOperator(OpDef: TOperatorDefinition);
    // @name will remove the @link(TOperatorDefinition) whose
    // @link(TOperatorDefinition.OperatorName) is OperatorName.
    procedure RemoveOperator(OperatorName: AnsiString);
    {
      See: @Link(TSpecialImplementorList).
    }
    property SpecialImplementorList: TSpecialImplementorList
      read FSpecialImplementorList;
    {
      @Name destroys all compiled @Link(TExpression)s in the
      @Link(TRbwParser).

      If several @Link(TRbwParser)s are used and variables owned by one
      are used in another one, ClearExpressions and
      @Link(ClearVariables) should be called
      in all of them before any of them is destroyed.
    }
    procedure ClearExpressions;
    {
      @Name removes all variables from the list of variables used by
      @Link(TRbwParser) but does not free them (because they may be used
      by another @Link(TRbwParser)).

      If several @Link(TRbwParser)s are used and variables owned by one
      are used in another one, @Link(ClearExpressions) and
      ClearVariables should be called
      in all of them before any of them is destroyed.
    }
    procedure ClearVariables;
    {
      @Name creates a @Link(TBooleanVariable)
      owned by @Link(TRbwParser) and allows
      it to be used with the @Link(TRbwParser).

      See also:  @link(RegisterVariable).
    }
    function CreateVariable(const AName, Classification: string;
      const Value: boolean; const NameToDisplay: string): TBooleanVariable; overload;
    function CreateVariable(const AName, Classification: string;
      const Value: boolean; VariableClass: TBooleanVariableClass; const NameToDisplay: string):
      TBooleanVariable; overload;
    {
      @Name creates a @Link(TIntegerVariable)
      owned by @Link(TRbwParser) and allows
      it to be used with the @Link(TRbwParser).

      See also:  @link(RegisterVariable).
    }
    function CreateVariable(const AName, Classification: string;
      const Value: integer; const NameToDisplay: string): TIntegerVariable; overload;
    function CreateVariable(const AName, Classification: string;
      const Value: integer; VariableClass: TIntegerVariableClass; const NameToDisplay: string)
      : TIntegerVariable; overload;
    {
      @Name creates a @Link(TRealVariable)
      owned by @Link(TRbwParser) and allows
      it to be used with the @Link(TRbwParser).

      See also:  @link(RegisterVariable).

      Example:
      @longcode(#
      procedure TForm1.Button2Click(Sender: TObject);
      var
        Index : integer;
        Variable : TRealVariable;
      begin
        RbwParser1.CreateVariable('A', 2.5);
        RbwParser1.CreateVariable('B', 3.5);
        RbwParser1.Compile('A + B');
        RbwParser1.CurrentExpression.Evaluate;
        Label1.Caption := 'result: '
          + FloatToStr(RbwParser1.CurrentExpression.DoubleResult);
        // Label1.Caption is set to "result: 6".
        Index := RbwParser1.IndexOfVariable('a');
        // variable names are not case sensitive.
        Variable := RbwParser1.Variables[Index] as TRealVariable;
        Variable.Value := 6.5;

        RbwParser1.CurrentExpression.Evaluate;
        Label2.Caption := 'result: '
          + FloatToStr(RbwParser1.CurrentExpression.DoubleResult);
        // Label2.Caption is set to "result: 10".
        // As it stands, this event handler would cause
        // an error if it was called twice because
        // it would try to create variables named "A" and "B" twice.
        // Calling ClearVariables would prevent this if that is
        // what you really want to do.
      end;
      #)
    }
    function CreateVariable(const AName, Classification: string;
      const Value: double; const NameToDisplay: string): TRealVariable; overload;
    function CreateVariable(const AName, Classification: string;
      const Value: double; VariableClass: TRealVariableClass; const NameToDisplay: string):
      TRealVariable; overload;
    {
      @Name creates a @Link(TStringVariable)
      owned by @Link(TRbwParser) and allows
      it to be used with the @Link(TRbwParser).

      See also:  @link(RegisterVariable).
    }
    function CreateVariable(const AName, Classification: string;
      const Value: string; const NameToDisplay: string): TStringVariable; overload;
    function CreateVariable(const AName, Classification: string;
      const Value: string; VariableClass: TStringVariableClass; const NameToDisplay: string):
      TStringVariable; overload;
    {
      @Name searches for AString in @Link(Expressions). If it finds it,
      it sets @Link(CurrentExpression) to that @Link(TExpression)
      and returns it's position in @Link(Expressions).  If it doesn't find it,
      Compile converts AString into a @Link(TExpression), sets
      @Link(CurrentExpression) to the @Link(TExpression) it created,
      adds the @Link(TExpression) to @Link(Expressions)
      and returns the position of the @Link(TExpression) in @Link(Expressions).

      Expressions are owned by @Link(TRbwParser).
    }
    function Compile(var AString: string): integer;
    {
      @Name is the most recently @Link(Compile)d @Link(TExpression).
    }
    property CurrentExpression: TExpression read FCurrentExpression;
    {
      @Name creates an instance of @Link(TRbwParser);
    }
    constructor Create(AOwner: TComponent); override;
    {
      @Name destroys the @Link(TExpression) at Index and deletes it
      from the list of @Link(Expressions).
    }
    procedure DeleteExpression(const Index: integer);
    {
      @Name destroys the @Link(TRbwParser).  Do not call Destroy directly.
      Call Free instead.
    }
    destructor Destroy; override;
    {
      @Name returns the number of @Link(TExpression)s in
      @Link(Expressions).
    }
    function ExpressionCount: integer;
    {
      @Name is an array of @Link(TExpression)s.
    }
    property Expressions[const Index: integer]: TExpression
    read GetExpressions;
    {
      @Name locates the @Link(TCustomVariable) whose name is
      VariableName in @Link(Variables) and returns it's position.  If it is
      not in @Link(Variables) it returns -1.
    }
    function IndexOfVariable(VariableName: string): integer;
    {
      @Name stores @Link(TFunctionClass)es in it's Objects property
      and manages the @Link(TFunctionClass)es used by TRbwParser.
    }
    property Functions: TFunctionStringList read FFunctions;
    {
      @Name allows a @Link(TCustomValue) to be used by
      the @Link(TRbwParser) even though the @Link(TCustomValue) isn't
      owned by the @Link(TRbwParser).
    }
    procedure RegisterVariable(const Value: TCustomValue);
    {
      @Name deletes Expression from the list of
      @Link(Expressions) and destroys it.
    }
    procedure RemoveExpression(const Expression: TExpression);
    {
      If Variable is owned by or registered with @Link(TRbwParser),
      @Name removes it and, if it is owned by the current
      instance of @Link(TRbwParser),
      also destroys it.  It also destroys any @link(TExpression)s in the
      current @Link(TRbwParser) that use Variable.
    }
    procedure RemoveVariable(const Variable: TCustomVariable);
    {
      @Name changes the name of the variable at position
      Index to a NewName and changes Index to the new position of
      the variable.
    }
    procedure RenameVariable(var Index: integer; NewName: string; NewDisplayName: string);
    {
      @Name is an array of variables (@Link(TCustomValue)s) used by
      the current @Link(TRbwParser).
    }
    property Variables[const Index: integer]: TCustomValue read GetVariable;
    {
      @Name is the number of variables in @Link(Variables).
    }
    function VariableCount: integer;
    { Public declarations }
  published
    { Published declarations }
  end;

  {
    @abstract(@Name is used for "if" and "case" statements. It allows
    Evaluate to be faster and safer by only evaluating the arguments
    that will be used.)
  }
  TSelectExpression = class(TExpression)
  private
    function GetSelectIndex(AVariable: TConstant): integer;
  public
    {
      @Name calls Evaluate for its first argument and based on
      the result of that argument calls Evaluate for one of its other
      arguments and sets its own result to the result of that argument.
    }
    procedure Evaluate; override;
  end;


  // @Name installs the component on the component palette.
procedure Register;

{
  @Name converts a @Link(TRbwDataType) to a string.  This function
  is primarily used in error messages.
}
function DataTypeToString(const DataType: TRbwDataType): string;

{
  @Name uses root to generate the name of a variable
  that could be accepted by @link(TRbwParser) but does check whether a variable
  with that name already exists.
}
function GenerateVariableName(const root: string): string;

function IdenticalFormulas(const Formula1, Formula2: string): boolean;

var
  {
    @abstract(@Name contains a series of @link(TFunctionClass)es
    that define overloaded functions.)

    The following functions are included by default.

    @unorderedList(
    @item(@bold(Abs)(Value) returns the absolute value of Value.
    Value can be either an integer or a real number.
    The value returned by Abs will have the same type as Value.)

    @item(@bold(Case)(Index, Result1, Result2, ...).
    Case uses Index to determine which of the Result1,
    Result2, ... arguments will be returned as a result.
    If Index equals 1, Result1 is returned; if Index equals 2,
    Result2 is returned; if Index equals 3, Result3 is returned;
    and so forth.  Only "Index", constant expressions
    and the result that is returned will be evaluated.
    The types of Result1, Result2, ... must all be the same
    but they can be of any type.  The type that is returned
    will be the same as the type of Result1, Result2, ...)

    @item(@bold(If)(Boolean_Value, If_True_Result, If_False_Result).
    If uses Boolean_Value to determine whether If_True_Result or
    If_False_Result is returned as a result.
    If Boolean_Value is true, If_True_Result is returned;
    if Boolean_Value is false, If_False_Result is returned.
    Only "Boolean_Value", constant expressions and the result
    that is returned will be evaluated.
    The types of If_True_Result and If_False_Result
    must be the same but they can be of any type.
    The type that is returned will be the same as the type of
    If_True_Result and If_False_Result.)

    @item(@bold(Max)(Value1, Value2, ...) returns whichever of its arguments
    is the largest.  Its arguments must be either integers or
    real numbers.  The result will be a real number if any of
    the arguments is a real number.  If all the arguments are
    integers, the result will be an integer.)

    @item(@bold(Min)(Value1, Value2, ...) returns whichever of its arguments
    is the smallest.  Its arguments must be either integers or
    real numbers.  The result will be a real number if any of
    the arguments is a real number.  If all the arguments are
    integers, the result will be an integer.)

    @item(@bold(MultiInterpolate)(Position, Value1, Distance1,
    [Value2, Distance2,] ...). If Position is less than or equal
    to Distance1, MultiInterpolate returns Value1.
    If Position is greater than or equal to DistanceN,
    MultiInterpolate returns ValueN.
    If  Position is between any two adjacent distances,
    linear interpolation between the associated values will be
    used to determine the value that will be returned.
    See also @link(TFunctionStringList Interpolate).)

    @item(@bold(Sqr)(Value) returns Value Squared. Value can be either an
    integer or a real number.  The result of Sqr will be an
    integer if Value is an integer.
    Otherwise it will be a real number.)
    )

  }
  OverloadedFunctionList: TObjectList;


//var
//  RbwParserSettings: TFormatSettings;

implementation

uses
{$IFDEF Delphi_XE_UP}
  Character,
{$ENDIF}
  Math, StrUtils;

resourcestring
  StrErrorARootNameM = 'Error: A root name must be supplied when generating ' +
  'a variable name.';
  StrErrorVariablesMus = 'Error: Variables must be named.';
  StrErrorSIsIlleg = 'Error: "%s" is illegal. The first character of a varia' +
  'ble name must be a letter or an underscore.';
  StrErrorSIsIlleg2 = 'Error: "%s" is illegal.  Variable names may only inclu' +
  'de letters, digits, or the underscore symbol.';
  StrErrorSIsIlleg3 = 'Error: "%s" is illegal.  Variable names may only inclu' +
  'de letters, digits, or the underscore symbol. The first character may not be a digit.';
  StrErrorAVariableNa = 'Error: A Variable named "%s" already exists.';
  StrErrorCreatingVariable = 'Error creating variable: A Variable named "%s" already exists. New display name = "%s". Existing display name = "%s".';
  StrEmptyFormula = 'empty formula';
  StrErrorUnterminated = 'Error: Unterminated string';
  StrErrorUnmatchedPar = 'Error: Unmatched parentheses';
  StrErrorAttemptToUsB = 'Error: attempt to use %s as a Boolean.';
  StrErrorAttemptToUsR = 'Error: attempt to use %s as a Real.';
  StrErrorAttemptToUsI = 'Error: attempt to use %s as an Integer.';
  StrErrorAttemptToUsS = 'Error: attempt to use %s as a String.';
  StrErrorTheStackIs = 'Error: The Stack is empty.';
  StrAReal = 'a Real';
  StrAnInteger = 'an Integer';
  StrABoolean = 'a Boolean';
  StrAString = 'a String';
  StrErrorThe0sFunc = 'Error: the %0:s function requires %1:d arguments but ' +
  'none were supplied.';
  StrErrorThe0sFuncOpt = 'Error: the %0:s function requires between %1:d and' +
  ' %2:d arguments but none were supplied.';
  StrErrorThe0sFunc2 = 'Error: the %0:s function requires at least %1:d argu' +
  'ments but %2:d were supplied.';
  StrErrorTheSFuncti3 = 'Error: the %s function requires at most %1:d argume' +
  'nts but %2:d were supplied.';
  StrErrorThe0sFunc4 = 'Error: the %0:s function requires at least %1:d argu' +
  'ments but %2:d were supplied.';
  StrErrorInArgumentNu = 'Error in argument number %0:d of %1:s %2:s; %3:s';
  StrParsingErrorCheck = 'Parsing Error; Check that no function or variable ' +
  'names have been misspelled.';
  StrErrorInParsingS = 'Error in parsing "%s" sign.';
  StrErrorInParsingSOp = 'Error in parsing "%s" operator.';
  StrErrorInParsing02 = 'Error in parsing "%0:1s" operator because %1:s is u' +
  'ndefined.';
  StrErrorInParsing03 = 'Error in parsing "%0:s" operator because of an erro' +
  'r in %1:s.';
  StrTheSOperatorMu = 'The "%s" operator must have at least one argument.';
  StrErrorInParsing0 = 'Error in parsing "%0:s" operator; It is not possible' +
  ' to apply the "%0:s" operator to %1:s.';
  StrErrorInParsingThe = 'Error in parsing the "%0:s" operator.  Check to ma' +
  'ke sure that all the variables in the formula have been defined.';
  StrErrorInParsing04 = 'Error in parsing "%0:s" operator because %1:s and %' +
  '2:s are undefined.  Check to make sure that all the variables in the form' +
  'ula have been defined.';
  StrErrorInParsing05 = 'Error in parsing "%0:s" operator because %1:s is un' +
  'defined.  Check to make sure that all the variables in the formula have b' +
  'een defined.';
  StrErrorInParsing06 = 'Error in parsing "%0:s" operator; It is not possibl' +
  'e to apply the "%0:s" operator to %1:s and %2:s.';
  StrError0sNotRe = 'Error: "%0:s" not recognized or the arguments for "%0:s' +
  '" are not of the correct types.';
  StrError0sCanNot = 'Error: %0:s can not be assigned to %1:s variable.';
  StrFactorialForValues = 'Factorial for values greater than 12 can not be c' +
  'omputed by the Factorial function';
  StrFactorialForValues170 = 'Factorial for values greater than 170 can not be ' +
  'computed by the FactorialR function';
  StrSCanNotBeConver = '%s can not be converted to a real number.';
  StrErrorTheFunction = 'Error: The function is of the incorrect type.';
  StrDIsOutOfRange = '%d is out of range.';
  StrErrorAFunctionNa = 'Error: A function named %s already exists.';
  StrErrorUnableToEva = 'Error: Unable to evaluate %0:s function when the fi' +
  'rst argument evaluates to %1:d because the first argument must always be ' +
  'positive.';
  StrErrorUnableToEva2 = 'Error: Unable to evaluate %0:s function when the f' +
  'irst argument evaluates to %1:d because the number of arguments supplied ' +
  'to the function was %2:d instead of at least %3:d.';
  StrUnableToEvaluate = 'Unable to evaluate "%0:s". ' + sLineBreak +
  'The error message was %1:s' + sLineBreak +
  'The class name was %2:s';

const
  ValidCharacters = ['A'..'Z', 'a'..'z', '0'..'9', '_'];
  ValidFirstCharacters = ['A'..'Z', 'a'..'z', '_'];

procedure Register;
begin
  RegisterComponents('RBW', [TRbwParser]);
end;

{$WARNINGS OFF}
function InternalStrToFloat(const AString: string): Extended;
var
  OldDecimalSeparator: Char;
begin
  {$IFDEF Delphi_2009_UP}
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  {$ELSE}
  OldDecimalSeparator := DecimalSeparator;
  {$ENDIF}
  try
    {$IFDEF Delphi_2009_UP}
    FormatSettings.DecimalSeparator := '.';
    {$ELSE}
    DecimalSeparator := '.';
    {$ENDIF}
    result := StrToFloat(AString);
  finally
    {$IFDEF Delphi_2009_UP}
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
    {$ELSE}
    DecimalSeparator := OldDecimalSeparator;
    {$ENDIF}
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function InternalFloatToStr(Value: Extended): string;
var
  OldDecimalSeparator: Char;
begin
  {$IFDEF Delphi_2009_UP}
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  {$ELSE}
  OldDecimalSeparator := DecimalSeparator;
  {$ENDIF}
  try
    {$IFDEF Delphi_2009_UP}
    FormatSettings.DecimalSeparator := '.';
    {$ELSE}
    DecimalSeparator := '.';
    {$ENDIF}
    result := FloatToStr(Value);
  finally
    {$IFDEF Delphi_2009_UP}
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
    {$ELSE}
    DecimalSeparator := OldDecimalSeparator;
    {$ENDIF}
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function GenerateVariableName(const root: string): string;
var
  Index: integer;
begin
  result := root;
  if Length(result) < 1 then
  begin
    raise ERbwParserError.Create(StrErrorARootNameM);
  end;
  for Index := 1 to Length(result) do
  begin
    {$IFDEF Delphi_2009_UP}
    if not CharInSet(result[Index], ValidCharacters) then
    {$ELSE}
    if not (result[Index] in ValidCharacters) then
    {$ENDIF}
    begin
      result[Index] := '_'
    end;
  end;
  {$IFDEF Delphi_2009_UP}
  if not CharInSet(result[1], ValidFirstCharacters) then
  {$ELSE}
  if not (result[1] in ValidFirstCharacters) then
  {$ENDIF}
  begin
    result := '_' + result;
  end;
end;
{$WARNINGS ON}

procedure ValidateVariableName(const VariableName: string);
{$IFNDEF Delphi_XE_UP}
var
  Index: integer;
{$ENDIF}
begin
  if Length(VariableName) < 1 then
  begin
    raise ERbwParserError.Create(StrErrorVariablesMus);
  end;
  {$IFDEF Delphi_XE_UP}
  if not IsValidIdent(VariableName, False) then
  begin
    raise ERbwParserError.Create(Format(StrErrorSIsIlleg3, [VariableName]));
  end;
  {$ELSE}
    {$IFDEF Delphi_2009_UP}
    if not CharInSet(VariableName[1], ValidFirstCharacters) then
    {$ELSE}
    if not (VariableName[1] in ValidFirstCharacters) then
    {$ENDIF}
    begin
      raise ERbwParserError.Create(Format(StrErrorSIsIlleg, [VariableName]));
    end;
    for Index := 2 to Length(VariableName) do
    begin
      {$IFDEF Delphi_2009_UP}
      if not CharInSet(VariableName[Index], ValidCharacters) then
      {$ELSE}
      if not (VariableName[Index] in ValidCharacters) then
      {$ENDIF}
      begin
        raise ERbwParserError.Create(Format(StrErrorSIsIlleg2, [VariableName]));
      end;
    end;
  {$ENDIF}
end;

type
  TIntegerStack = class(TObject)
  private
    FList: TList;
    function GetCapacity: integer;
    procedure SetCapacity(const Value: integer);
  public
    property Capacity: integer read GetCapacity write SetCapacity;
    procedure Clear;
    function Count: integer;
    constructor Create;
    destructor Destroy; override;
    function Peek: Integer;
    function Pop: integer;
    procedure Push(const AnInteger: integer);
  end;

  TTokenStringList = class(TStringList)
  private
    FOpereratorDefinitions: TList;
    function FixToken(const Token: string): string;
    procedure MakeVariableList(SpecialImplementorList: TSpecialImplementorList;
      Start, Stop: integer);
    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    function Compile(const Functions: TFunctionStringList;
      const Variables: TStringList;
      SpecialImplementorList: TSpecialImplementorList): TExpression;
    procedure EvaluateOverloadedFunctions(
      SpecialImplementorList: TSpecialImplementorList; Start: integer;
      var Stop: integer);
    procedure EvaluateSpecialImplementors(
      SpecialImplementorList: TSpecialImplementorList;
      Start: integer; var Stop: integer);
    function IsSign(Index: Integer; const Token: string): Boolean;
    procedure EvaluateUnaryOperator(
      SpecialImplementorList: TSpecialImplementorList;
      OperatorDefinition: TOperatorDefinition; var Stop: Integer; var Index: Integer);
    procedure EvaluateBinaryOperator(
      SpecialImplementorList: TSpecialImplementorList;
      OperatorDefinition: TOperatorDefinition;
      var Stop: Integer; var Index: Integer);
    procedure EvaluateOperators(SpecialImplementorList: TSpecialImplementorList;
      Start: integer; var Stop: integer);
  end;

  TSignOperator = class(TOperator)
  private
    function DecompileByType(DecompileType: TDecompileType): string;
  public
    {
      Decompile converts the value stored in the TSignOperator to a
      string that can be compiled into an equivalent TSignOperator
      together with its argument.
    }
    function Decompile: string; override;
    function DecompileDisplay: string; override;
  end;

  TIntToDoubleExpression = class(TExpression)
  private
    function DecompileByType(DecompileType: TDecompileType): string;
  protected
    // @name calls @name for the integer it is converting.
    procedure MakeDiagram(List: TStringList; Level: integer); override;
  public
    {
      Decompile returns the Decompile of the single argument
      of the TIntToDoubleExpression.
    }
    function Decompile: string; override;
    function DecompileDisplay: string; override;
  end;

  TPartialEvaluationOperator = class(TOperator)
  private
    function GetResult: Boolean;
    procedure SetResultFromVariable(AVariable: TConstant);
  protected
    //function MakeLinkedList(Prior: TExpression): TExpression; override;
  end;

  TAndExpression = class(TPartialEvaluationOperator)
  public
    procedure Evaluate; override;
  end;

  TOrExpression = class(TPartialEvaluationOperator)
  public
    procedure Evaluate; override;
  end;

  TMultiInterpolateExpression = class(TSelectExpression)
  private
    procedure SetResultFromNumber(ANumber: Double);
  public
    procedure Evaluate; override;
  end;

  TVariableExpression = class(TExpression)
  private
    function DecompileByType(DecompileType: TDecompileType): string;
  protected
    procedure MakeDiagram(List: TStringList; Level: integer); override;
  public
    constructor Create(const Variable: TCustomValue;
      SpecialImplementorList: TSpecialImplementorList);
    function Decompile: string; override;
    function DecompileDisplay: string; override;
    procedure Evaluate; override;
  end;

const
  Digits: set of AnsiChar = ['0'..'9'];
  Parenthesis: set of AnsiChar = ['(', ')'];
{$IFNDEF DELPHI_XE_UP}
  WhiteSpace: set of AnsiChar = [' ', #9, #10, #13, #32];
{$ENDIF}

var
  OperatorList: TObjectList;

  IntToDoubleFunction: TFunctionClass;

  NotOperator: TFunctionClass;
  PowerOperator: TFunctionClass;
  PowerOperator2: TFunctionClass;
  TimesIOperator: TFunctionClass;
  TimesROperator: TFunctionClass;
  DivideIOperator: TFunctionClass;
  DivideROperator: TFunctionClass;
  DivOperator: TFunctionClass;
  ModOperator: TFunctionClass;
  AndOperator: TFunctionClass;
  OrOperator: TFunctionClass;
  PlusSignIOperator: TFunctionClass;
  PlusSignROperator: TFunctionClass;
  MinusSignIOperator: TFunctionClass;
  MinusSignROperator: TFunctionClass;
  PlusIOperator: TFunctionClass;
  PlusROperator: TFunctionClass;
  PlusSOperator: TFunctionClass;
  MinusIOperator: TFunctionClass;
  MinusROperator: TFunctionClass;
  EqualIOperator: TFunctionClass;
  EqualROperator: TFunctionClass;
  EqualSOperator: TFunctionClass;
  EqualBOperator: TFunctionClass;
  NotEqualIOperator: TFunctionClass;
  NotEqualROperator: TFunctionClass;
  NotEqualSOperator: TFunctionClass;
  NotEqualBOperator: TFunctionClass;
  LessThanBOperator: TFunctionClass;
  LessThanIOperator: TFunctionClass;
  LessThanROperator: TFunctionClass;
  LessThanSOperator: TFunctionClass;
  GreaterThanBOperator: TFunctionClass;
  GreaterThanIOperator: TFunctionClass;
  GreaterThanROperator: TFunctionClass;
  GreaterThanSOperator: TFunctionClass;
  LessThanOrEqualsBOperator: TFunctionClass;
  LessThanOrEqualsIOperator: TFunctionClass;
  LessThanOrEqualsROperator: TFunctionClass;
  LessThanOrEqualsSOperator: TFunctionClass;
  GreaterThanOrEqualsBOperator: TFunctionClass;
  GreaterThanOrEqualsIOperator: TFunctionClass;
  GreaterThanOrEqualsROperator: TFunctionClass;
  GreaterThanOrEqualsSOperator: TFunctionClass;
  XorOperator: TFunctionClass;

  AbsIOverloadedFunction: TFunctionClass;
  AbsROverloadedFunction: TFunctionClass;
  CaseBOverloadedFunction: TFunctionClass;
  CaseIOverloadedFunction: TFunctionClass;
  CaseROverloadedFunction: TFunctionClass;
  CaseSOverloadedFunction: TFunctionClass;
  IfBOverloadedFunction: TFunctionClass;
  IfIOverloadedFunction: TFunctionClass;
  IfROverloadedFunction: TFunctionClass;
  IfSOverloadedFunction: TFunctionClass;
  MaxIOverloadedFunction: TFunctionClass;
  MaxROverloadedFunction: TFunctionClass;
  MinIOverloadedFunction: TFunctionClass;
  MinROverloadedFunction: TFunctionClass;
  SqrIOverloadedFunction: TFunctionClass;
  SqrROverloadedFunction: TFunctionClass;
  MultiInterpolateFunction: TFunctionClass;

var
  AbsFunctionI: TFunctionRecord;
  AbsFunctionR: TFunctionRecord;
  ArcCosFunction: TFunctionRecord;
  ArcCoshFunction: TFunctionRecord;
  ArcSinFunction: TFunctionRecord;
  ArcSinhFunction: TFunctionRecord;
  ArcTanFunction: TFunctionRecord;
  ArcTan2Function: TFunctionRecord;
  ArcTanhFunction: TFunctionRecord;

  CaseBooleanFunction: TFunctionRecord;
  CaseIntegerFunction: TFunctionRecord;
  CaseDoubleFunction: TFunctionRecord;
  CaseStringFunction: TFunctionRecord;
  ClosestFunction: TFunctionRecord;

  CopyFunction: TFunctionRecord;
  CosFunction: TFunctionRecord;
  CoshFunction: TFunctionRecord;
  DegToRadFunction: TFunctionRecord;
  ExpFunction: TFunctionRecord;
  FactorialFunction: TFunctionRecord;
  FactorialFFunction: TFunctionRecord;
  FloatToStrFunction: TFunctionRecord;
  FracFunction: TFunctionRecord;
  IfBooleanFunction: TFunctionRecord;
  IfIntegerFunction: TFunctionRecord;
  IfRealFunction: TFunctionRecord;
  IfStringFunction: TFunctionRecord;
  IntPowerFunction: TFunctionRecord;
  IntToStrFunction: TFunctionRecord;
  LengthFunction: TFunctionRecord;
  LnFunction: TFunctionRecord;
  Log10Function: TFunctionRecord;
  LogNFunction: TFunctionRecord;
  LowerCaseFunction: TFunctionRecord;
  MaxIFunction: TFunctionRecord;
  MaxRFunction: TFunctionRecord;
  MinIFunction: TFunctionRecord;
  MinRFunction: TFunctionRecord;
  OddFunction: TFunctionRecord;
  PiFunction: TFunctionRecord;
  PowerFunction: TFunctionRecord;
  PosFunction: TFunctionRecord;
  PosExFunction: TFunctionRecord;
  PositionInList: TFunctionRecord;
  RadToDegFunction: TFunctionRecord;
  RoundFunction: TFunctionRecord;
  SinFunction: TFunctionRecord;
  SinhFunction: TFunctionRecord;
  SqrIFunction: TFunctionRecord;
  SqrRFunction: TFunctionRecord;
  SqrtFunction: TFunctionRecord;
  StrToIntFunction: TFunctionRecord;
  StrToIntDefFunction: TFunctionRecord;
  StrToFloatFunction: TFunctionRecord;
  StrToFloatDefFunction: TFunctionRecord;
  TanFunction: TFunctionRecord;
  TanhFunction: TFunctionRecord;
  TrimMunction: TFunctionRecord;
  TruncFunction: TFunctionRecord;
  UpperCaseFunction: TFunctionRecord;

  InterpolateFunction: TFunctionRecord;
  DistanceFunction: TFunctionRecord;

function DataTypeToString(const DataType: TRbwDataType): string;
begin
  case DataType of
    rdtDouble:
      begin
        result := StrAReal;
      end;
    rdtInteger:
      begin
        result := StrAnInteger;
      end;
    rdtBoolean:
      begin
        result := StrABoolean;
      end;
    rdtString:
      begin
        result := StrAString;
      end;
  else
    Assert(False);
  end;

end;

function IsStringConstant(const Token: string): boolean;
begin
  result := (Length(Token) > 0) and (Token[1] = '"');
end;

function IsBooleanConstant(const Token: string; var Value: boolean): boolean;
var
  Temp: string;
begin
  Temp := UpperCase(Token);
  if Temp = 'TRUE' then
  begin
    result := True;
    Value := True;
  end
  else if Temp = 'FALSE' then
  begin
    result := True;
    Value := False;
  end
  else
  begin
    result := False;
  end;
end;

function IsIntegerConstant(const Token: string; var Value: integer): boolean;
var
  Code: integer;
begin
  Assert(Length(Token) >= 1);
  // Val('x1', Value, Code) will convert to a numeric value.
  // Use this test to prevent that problem.
  {$IFDEF Delphi_2009_UP}
  result := CharInSet(Token[1], ['+', '-', '0', '1','2','3','4','5','6','7','8','9']);
  {$ELSE}
  result := Token[1] in ['+', '-', '0', '1','2','3','4','5','6','7','8','9'];
  {$ENDIF}
  if result then
  begin
    Val(Token, Value, Code);
    result := Code = 0;
  end;
end;

function IsFloatConstant(const Token: string; var Value: double): boolean;
var
  Code: integer;
begin
  Assert(Length(Token) >= 1);
  {$IFDEF Delphi_2009_UP}
  result := CharInSet(Token[1], ['+', '-', '0'..'9', '.']);
  {$ELSE}
  result := Token[1] in ['+', '-', '0'..'9', '.'];
  {$ENDIF}
  if result then
  begin
    Val(Token, Value, Code);
    result := Code = 0;
  end;
end;

{ TRbwParser }

function TRbwParser.CreateVariable(const AName, Classification: string;
  const Value: integer; VariableClass: TIntegerVariableClass; const NameToDisplay: string)
  : TIntegerVariable;
var
  VarIndex: Integer;
begin
  VarIndex := FVariables.IndexOf(Trim(UpperCase(AName)));
  if VarIndex >= 0 then
  begin
    raise ErbwParserError.CreateMode(Format(StrErrorCreatingVariable,
      [AName, NameToDisplay, Variables[VarIndex].DecompileDisplay]), 1);
  end;
  VarIndex := FVariables.IndexOf(Trim(UpperCase(NameToDisplay)));
  if (NameToDisplay <> '') and (AnsiCompareText(NameToDisplay, AName) <> 0)
    and (VarIndex >= 0) then
  begin
    raise ErbwParserError.CreateMode(Format(StrErrorCreatingVariable,
      [AName, NameToDisplay, Variables[VarIndex].DecompileDisplay]), 1);
  end;
  result := VariableClass.Create(AName, NameToDisplay);
  result.Classification := Classification;
  result.Value := Value;
  FVariables.AddObject(result.Name, result);
  if result.Name <> result.DisplayName then
  begin
    FVariables.AddObject(result.DisplayName, result);
  end;
  FOwnedVariables.Add(result);
end;

function TRbwParser.CreateVariable(const AName, Classification: string;
  const Value: integer; const NameToDisplay: string): TIntegerVariable;
begin
  result := CreateVariable(AName, Classification, Value, TIntegerVariable, NameToDisplay);
end;

function TRbwParser.CreateVariable(const AName, Classification: string;
  const Value: boolean; VariableClass: TBooleanVariableClass; const NameToDisplay: string):
  TBooleanVariable;
var
  VarIndex: Integer;
begin
  VarIndex := FVariables.IndexOf(Trim(UpperCase(AName)));
  if VarIndex >= 0 then
  begin
    raise ErbwParserError.CreateMode(Format(StrErrorCreatingVariable,
      [AName, NameToDisplay, Variables[VarIndex].DecompileDisplay]), 1);
  end;
  VarIndex := FVariables.IndexOf(Trim(UpperCase(NameToDisplay)));
  if (NameToDisplay <> '') and (AnsiCompareText(NameToDisplay, AName) <> 0)
    and (VarIndex >= 0) then
  begin
    raise ErbwParserError.CreateMode(Format(StrErrorCreatingVariable,
      [AName, NameToDisplay, Variables[VarIndex].DecompileDisplay]), 1);
  end;
  result := VariableClass.Create(AName, NameToDisplay);
  result.Classification := Classification;
  result.Value := Value;
  FVariables.AddObject(result.Name, result);
  if result.Name <> result.DisplayName then
  begin
    FVariables.AddObject(result.DisplayName, result);
  end;
  FOwnedVariables.Add(result);
end;


function TRbwParser.CreateVariable(const AName, Classification: string;
  const Value: boolean; const NameToDisplay: string): TBooleanVariable;
begin
  result := CreateVariable(AName, Classification, Value, TBooleanVariable, NameToDisplay);
end;

function TRbwParser.CreateVariable(const AName, Classification: string;
  const Value: string; VariableClass: TStringVariableClass; const NameToDisplay: string):
  TStringVariable;
var
  VarIndex: Integer;
begin
  VarIndex := FVariables.IndexOf(Trim(UpperCase(AName)));
  if VarIndex >= 0 then
  begin
    raise ErbwParserError.CreateMode(Format(StrErrorCreatingVariable,
      [AName, NameToDisplay, Variables[VarIndex].DecompileDisplay]), 1);
  end;
  VarIndex := FVariables.IndexOf(Trim(UpperCase(NameToDisplay)));
  if (NameToDisplay <> '') and (AnsiCompareText(NameToDisplay, AName) <> 0)
    and (VarIndex >= 0) then
  begin
    raise ErbwParserError.CreateMode(Format(StrErrorCreatingVariable,
      [AName, NameToDisplay, Variables[VarIndex].DecompileDisplay]), 1);
  end;
  result := VariableClass.Create(AName, NameToDisplay);
  result.Classification := Classification;
  result.Value := Value;
  FVariables.AddObject(result.Name, result);
  if result.Name <> result.DisplayName then
  begin
    FVariables.AddObject(result.DisplayName, result);
  end;
  FOwnedVariables.Add(result);
end;

function TRbwParser.CreateVariable(const AName, Classification,
  Value: string; const NameToDisplay: string): TStringVariable;
begin
  result := CreateVariable(AName, Classification, Value, TStringVariable, NameToDisplay);
end;

function TRbwParser.CreateVariable(const AName, Classification: string;
  const Value: double; const NameToDisplay: string): TRealVariable;
begin
  result := CreateVariable(AName, Classification, Value, TRealVariable, NameToDisplay);
end;

function TRbwParser.CreateVariable(const AName, Classification: string;
  const Value: double; VariableClass: TRealVariableClass; const NameToDisplay: string):
  TRealVariable;
var
  VarIndex: Integer;
begin
  VarIndex := FVariables.IndexOf(Trim(UpperCase(AName)));
  if VarIndex >= 0 then
  begin
    raise ErbwParserError.CreateMode(Format(StrErrorCreatingVariable,
      [AName, NameToDisplay, Variables[VarIndex].DecompileDisplay]), 1);
  end;
  VarIndex := FVariables.IndexOf(Trim(UpperCase(NameToDisplay)));
  if (NameToDisplay <> '') and (AnsiCompareText(NameToDisplay, AName) <> 0)
    and (VarIndex >= 0) then
  begin
    raise ErbwParserError.CreateMode(Format(StrErrorCreatingVariable,
      [AName, NameToDisplay, Variables[VarIndex].DecompileDisplay]), 1);
  end;
  result := VariableClass.Create(AName, NameToDisplay);
  result.Classification := Classification;
  result.Value := Value;
  FVariables.AddObject(result.Name, result);
  if result.Name <> result.DisplayName then
  begin
    FVariables.AddObject(result.DisplayName, result);
  end;
  FOwnedVariables.Add(result);
end;

{ TArgumentList }

procedure TArgumentList.Add(Item: TOperatorArgumentDefinition);
begin
  FList.Add(Item);
end;

constructor TArgumentList.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

destructor TArgumentList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TArgumentList.GetCount: integer;
begin
  result := FList.Count;
end;

function TArgumentList.GetItems(Index: integer): TOperatorArgumentDefinition;
begin
  result := TOperatorArgumentDefinition(FList[Index]);
end;

function IdenticalFormulas(const Formula1, Formula2: string): boolean;
var
  StringLiteral1: string;
  StringLiteral2: string;
  FirstQuotePos: Integer;
  SecondQuotePos: Integer;
begin
  if SameText(Formula1, Formula2) then
  begin
    if Pos('"', Formula1) = 0 then
    begin
      result := True;
    end
    else
    begin
      result := True;

      FirstQuotePos := Pos('"', Formula1);
      SecondQuotePos := PosEx('"', Formula1, Succ(FirstQuotePos));
      While (FirstQuotePos > 0) and (SecondQuotePos > 0) do
      begin
        StringLiteral1 := Copy(Formula1, FirstQuotePos+1, SecondQuotePos-FirstQuotePos-1);
        StringLiteral2 := Copy(Formula2, FirstQuotePos+1, SecondQuotePos-FirstQuotePos-1);
        result := StringLiteral1 = StringLiteral2;
        if not result then
        begin
          Exit;
        end;
        FirstQuotePos := PosEx('"', Formula1, Succ(SecondQuotePos));
        SecondQuotePos := PosEx('"', Formula1, Succ(FirstQuotePos));
      end;
    end;
  end
  else
  begin
    result := False;
  end;
end;

function TRbwParser.Compile(var AString: string): integer;
var
  Index: integer;
  StringConstants: TStringList;
  IsStringConstantA: boolean;
  LastPosition: integer;
  LineIndex: integer;
  ALine: string;
  IsWhiteSpaceChar: boolean;
  OperatorIndex: integer;
  AnOperator: string;
  TestString: string;
  Skip: integer;
  Level: integer;
  Tokens: TTokenStringList;
//  OldDecimalSeparator: Char;
begin
  AString := (Trim(AString));
  if AString = '' then
  begin
    result := -1;
    raise ERbwParserError.Create(StrEmptyFormula);
    Exit;
  end;

  if (FCachedFindResult >= 0)
    and (FCachedFindResult < FExpressions.Count) then
  begin
    if IdenticalFormulas(AString, FExpressions[FCachedFindResult]) then
    begin
      result := FCachedFindResult;
      FCurrentExpression := Expressions[result];
      AString := FCurrentExpression.Decompile;
      Exit;
    end;
  end;

  if FExpressions.Find(AString, result)
    and (IdenticalFormulas(AString, FExpressions[result])) then
  begin
    FCurrentExpression := Expressions[result];
    FCachedFindResult := result;
    AString := FCurrentExpression.Decompile;
  end
  else
  begin
    FCurrentExpression := nil;
    Tokens := TTokenStringList.Create;
    try
      Tokens.FOpereratorDefinitions := FOpereratorDefinitions;
      // Find string constants (starting with the " character.
      // For now, at least, string constants may not contain the " character.
      LastPosition := 0;
      IsStringConstantA := False;
      StringConstants := TStringList.Create;
      try
        for Index := 1 to Length(AString) do
        begin
          if AString[Index] = '"' then
          begin
            IsStringConstantA := not IsStringConstantA;
            if (Index > 1) then
            begin
              if IsStringConstantA then
              begin
                StringConstants.Add(UpperCase(Copy(AString, LastPosition + 1,
                  Index - LastPosition - 1)));
              end
              else
              begin
                StringConstants.Add(Copy(AString, LastPosition,
                  Index - LastPosition + 1));
              end;
            end;
            LastPosition := Index;
          end;
        end;
        if LastPosition < Length(AString) then
        begin
          if IsStringConstantA then
          begin
            // raise error: unterminated string
            raise ErbwParserError.Create(StrErrorUnterminated);
          end
          else
          begin
            StringConstants.Add(Copy(AString, LastPosition + 1,
              Length(AString) - LastPosition));
          end;
        end;
        // At this point StringConstants contains string constants
        // and text that requires further processing.
        for LineIndex := 0 to StringConstants.Count - 1 do
        begin
          ALine := StringConstants[LineIndex];
          // If the current line is a string constant, add it to the tokens and
          // then go on to the next line.
          if Length(ALine) > 0 then
          begin
            if ALine[1] = '"' then
            begin
              Tokens.Add(ALine);
              Continue;
            end;
          end;
          // ALine is not a string constant. Extract tokens from it.
          // LastPosition is used to identify the beginning of the current token.
          // LastPosition is one space before the beginning of the current token.
          LastPosition := 0;
          ALine := Trim(ALine);
          IsWhiteSpaceChar := True;
          Skip := 0;
          for Index := 1 to Length(ALine) do
          begin
            // if we found the beginning of an operator in the last character and the
            // length of the operator was > 1, skip this character.
            if Skip > 0 then
            begin
              LastPosition := Index;
              Dec(Skip);
              Continue;
            end;
            // test if this character is white space.
            {$IFDEF Delphi_XE_UP}
            if IsWhiteSpace(ALine, Index) then
            {$ELSE}
            {$IFDEF Delphi_2009_UP}
            if CharInSet(ALine[Index], WhiteSpace) then
            {$ELSE}
            if ALine[Index] in WhiteSpace then
            {$ENDIF}
            {$ENDIF}
            begin
              // if this character is white space, but the previous character wasn't
              // white space, the last character must be the end of a token.
              if not IsWhiteSpaceChar then
              begin
                Tokens.Add(copy(ALine, LastPosition + 1, Index - LastPosition -
                  1));
              end;
              IsWhiteSpaceChar := True;
              LastPosition := Index;
              Continue;
            end
            else
            begin
              IsWhiteSpaceChar := False;
            end;
            // The current character is not white space.
            // Add parentheses to the tokens.
            {$IFDEF Delphi_2009_UP}
            if CharInSet(ALine[Index], Parenthesis) then
            {$ELSE}
            if ALine[Index] in Parenthesis then
            {$ENDIF}
            begin
              Tokens.Add(copy(ALine, LastPosition + 1, Index - LastPosition -
                1));
              Tokens.Add(ALine[Index]);
              LastPosition := Index;
              Continue;
            end;
            // See if the current character is the beginning of an operator.
            // If so, add the prior token and the operator to the list of tokens.

            for OperatorIndex := 0 to WordOperators.Count - 1 do
            begin
              AnOperator := WordOperators[OperatorIndex];
              TestString := Copy(ALine, Index, Length(AnOperator));
              if AnOperator = TestString then
              begin
                {$IFDEF Delphi_2009_UP}
                if ((Index = 1) or CharInSet(ALine[Index - 1], [' ', ')', ',']))
                  and ((Index + Length(AnOperator) > Length(ALine))
                  or CharInSet(ALine[Index + Length(AnOperator)], [' ', '(', ',']))
                  or not CharInSet(AnOperator[1], ['A'..'Z', 'a'..'z']) then
                {$ELSE}
                if ((Index = 1) or (ALine[Index - 1] in [' ', ')', ',']))
                  and ((Index + Length(AnOperator) > Length(ALine))
                  or (ALine[Index + Length(AnOperator)] in [' ', '(', ',']))
                  or not (AnOperator[1] in ['A'..'Z', 'a'..'z']) then
                {$ENDIF}
                begin
                  Tokens.Add(copy(ALine, LastPosition + 1, Index - LastPosition
                    - 1));
                  Tokens.Add(TestString);
                  LastPosition := Index;
                  // if the length of the token is greater than 1, skip characters
                  // until after the end of the operator is reached.
                  Skip := Length(TestString) - 1;
                  break;
                end;
              end;
            end;
            if Skip > 0 then
            begin
              Continue;
            end;
            for OperatorIndex := 0 to Operators.Count - 1 do
            begin
              AnOperator := Operators[OperatorIndex];
              TestString := Copy(ALine, Index, Length(AnOperator));
              if AnOperator = TestString then
              begin
                {$IFDEF Delphi_2009_UP}
                if (Index > 1)
                  and ((AnOperator = '-') or (AnOperator = '+'))
                  and CharInSet(ALine[Index - 1], ['e', 'E']) then
                {$ELSE}
                if (Index > 1)
                  and ((AnOperator = '-') or (AnOperator = '+'))
                  and (ALine[Index - 1] in ['e', 'E']) then
                {$ENDIF}
                begin
                  try
                    InternalStrToFloat(Copy(ALine, LastPosition + 1, Index -
                      LastPosition + 1));
                    break;
                  except on EConvertError do
                    begin
                      // do nothing = don't break;
                    end;
                  end;

                end;

                Tokens.Add(copy(ALine, LastPosition + 1, Index - LastPosition -
                  1));
                Tokens.Add(TestString);
                LastPosition := Index;
                // if the length of the token is greater than 1, skip characters
                // until after the end of the operator is reached.
                Skip := Length(TestString) - 1;
                break;
              end;
            end;
            AnOperator := ',';
            TestString := Copy(ALine, Index, Length(AnOperator));
            if AnOperator = TestString then
            begin
              {$IFDEF Delphi_2009_UP}
              if (Index > 1)
                and ((AnOperator = '-') or (AnOperator = '+'))
                and CharInSet(ALine[Index - 1], ['e', 'E']) then
              {$ELSE}
              if (Index > 1)
                and ((AnOperator = '-') or (AnOperator = '+'))
                and (ALine[Index - 1] in ['e', 'E']) then
              {$ENDIF}
              begin
                try
                  InternalStrToFloat(Copy(ALine, LastPosition + 1, Index -
                      LastPosition));
                  break;
                except on EConvertError do
                  begin
                    // do nothing = don't break;
                  end;
                end;

              end
              else if (Index <= 1) and (LineIndex = 0) then
              begin
                raise ERbwParserError.Create('A formula can not begin with ",".');
              end;

              Tokens.Add(copy(ALine, LastPosition + 1, Index - LastPosition -
                1));
              Tokens.Add(TestString);
              LastPosition := Index;
              // if the length of the token is greater than 1, skip characters
              // until after the end of the operator is reached.
              Skip := Length(TestString) - 1;
            end;
          end;
          // Add last token
          if LastPosition < Length(ALine) then
          begin
            Tokens.Add(Copy(ALine, LastPosition + 1, Length(ALine) -
              LastPosition));
          end;
        end;
      finally
        StringConstants.Free;
      end;
      Level := 0;
      for LineIndex := 0 to Tokens.Count - 1 do
      begin
        ALine := Tokens[LineIndex];
        if not IsStringConstant(ALine) then
        begin
          if ALine = '(' then
          begin
            Inc(Level)
          end;
          if ALine = ')' then
          begin
            Dec(Level)
          end;
          if Level < 0 then
          begin
            raise ErbwParserError.Create(StrErrorUnmatchedPar);
          end;
        end;
      end;
      if Level <> 0 then
      begin
        raise ErbwParserError.Create(StrErrorUnmatchedPar);
      end;
        try
          FCurrentExpression := Tokens.Compile(FFunctions, FVariables,
            self.SpecialImplementorList);
        except on E: ErbwParserError do
          begin
          {$IFDEF FPC}
            raise ErbwParserError.Create(
              Format(StrUnableToEvaluate, [AString, E.Message, E.ClassName]));
          {$ELSE}
            Exception.RaiseOuterException(ErbwParserError.Create(
              Format(StrUnableToEvaluate, [AString, E.Message, E.ClassName])));
          {$ENDIF}
          end;
        end;
      if FCurrentExpression <> nil then
      begin
        AString := FCurrentExpression.Decompile;
        if FExpressions.Find(AString, result)
          and (IdenticalFormulas(AString, FExpressions[result])) then
        begin
          Assert(FCurrentExpression.ResultType = Expressions[Result].ResultType);
          FCurrentExpression.Free;
          FCurrentExpression := Expressions[Result];
          FCachedFindResult := result;
        end
        else
        begin
          result := FExpressions.AddObject(AString, FCurrentExpression);
          FCachedFindResult := result;
        end;
      end
      else
      begin
        result := -1;
      end;

    finally
      Tokens.Free;
    end;
  end;
end;

constructor TRbwParser.Create(AOwner: TComponent);
begin
  inherited;
  FOwnedVariables := TObjectList.Create;
  FExpressions := TStringList.Create;
  FExpressions.Sorted := True;
  FExpressions.Duplicates := dupAccept;
  FExpressions.CaseSensitive := True;
  FFunctions := TFunctionStringList.Create;
  FVariables := TStringList.Create;
  FVariables.Sorted := True;
  FVariables.CaseSensitive := False;
  FVariables.Duplicates := dupIgnore;
  FSpecialImplementorList := TSpecialImplementorList.Create;

  FOpereratorDefinitions := TObjectList.Create;
  Operators := TStringList.Create;
  WordOperators := TStringList.Create;

  DefineNotOperator;
  DefinePowerOperator;
  DefinePowerOperator2;
  DefinePlusSignOperator;
  DefineMinusSignOperator;
  DefineTimesOperator;
  DefineDivideOperator;
  DefineDivOperator;
  DefineModOperator;
  DefineAndOperator;
  DefinePlusOperator;
  DefineMinusOperator;
  DefineOrOperator;
  DefineXorOperator;
  DefineEqualsOperator;
  DefineNotEqualsOperator;
  DefineLessThanOperator;
  DefineGreaterThanOperator;
  DefineLessThanOrEqualsOperator;
  DefineGreaterThanOrEqualsOperator;


end;

destructor TRbwParser.Destroy;
var
  Index: integer;
begin
  for Index := 0 to FExpressions.Count - 1 do
  begin
    FExpressions.Objects[Index].Free;
  end;
  FExpressions.Free;
  FFunctions.Free;
  FVariables.Free;
  FOwnedVariables.Free;
  FSpecialImplementorList.Free;
  FOpereratorDefinitions.Free;
  Operators.Free;
  WordOperators.Free;
  inherited;
end;

function TRbwParser.GetExpressions(const Index: integer): TExpression;
begin
  result := FExpressions.Objects[Index] as TExpression;
end;

function TRbwParser.GetVariable(const Index: integer): TCustomValue;
begin
  result := FVariables.Objects[Index] as TCustomValue;
end;

function TRbwParser.IndexOfVariable(VariableName: string): integer;
begin
  VariableName := Trim(UpperCase(VariableName));
  if not FVariables.Find(VariableName, result) then
  begin
    result := -1;
  end;
end;

function TRbwParser.VariableCount: integer;
begin
  result := FVariables.Count;
end;

procedure TRbwParser.RegisterVariable(const Value: TCustomValue);
var
  DisplayName: string;
begin
  if (Value is TBooleanVariable) or (Value is TIntegerVariable)
    or (Value is TRealVariable) or (Value is TStringVariable) then
  begin
    if FVariables.IndexOf(Value.Name) >= 0 then
    begin
      raise ErbwParserError.CreateMode('Error: A Variable named '
        + Value.Name + ' already exists', 1);
    end
    else if (Value is TCustomVariable)
      and (FVariables.IndexOf(TCustomVariable(Value).DisplayName) >= 0) then
    begin
      raise ErbwParserError.CreateMode('Error: A Variable named '
        + TCustomVariable(Value).DisplayName + ' already exists', 1);
    end
    else
    begin
      FVariables.AddObject(Value.Name, Value);
      if (Value is TCustomVariable) then
      begin
        DisplayName := TCustomVariable(Value).DisplayName;
        if DisplayName <> Value.Name then
        begin
          FVariables.AddObject(DisplayName, Value);
        end;
      end;
    end;
  end
  else
  begin
    raise ERbwParserError.Create('Error: Invalid variable type.');
  end;
end;

procedure TRbwParser.ClearExpressions;
var
  Index: integer;
begin
  for Index := 0 to FExpressions.Count - 1 do
  begin
    FExpressions.Objects[Index].Free;
  end;
  FExpressions.Clear;
  FCurrentExpression := nil;
end;

procedure TRbwParser.ClearVariables;
begin
  FVariables.Clear;
end;

procedure TRbwParser.RenameVariable(var Index: integer; NewName: string;
  NewDisplayName: string);
var
  AVariable: TCustomValue;
  NewExpressions: TStringList;
  ExpressionIndex: integer;
  AnExpression: TExpression;
  NewUserName: string;
  Position: integer;
//  NewUserDislayName: string;
begin
  NewUserName := Trim(NewName);
  if NewDisplayName = '' then
  begin
    NewDisplayName := NewUserName;
  end;
  NewName := NewUserName;
  NewDisplayName := Trim(NewDisplayName);
  ValidateVariableName(NewName);
  ValidateVariableName(NewDisplayName);
  if UpperCase(Variables[Index].Decompile) <> UpperCase(NewDisplayName) then
  begin
    Position := FVariables.IndexOf(NewDisplayName);
    if (Position >= 0) and (Position <> Index) then
    begin
      raise ErbwParserError.CreateMode(Format(StrErrorAVariableNa, [NewDisplayName]), 1);
    end;
    Position := FVariables.IndexOf(NewName);
    if (Position >= 0) and (Position <> Index) then
    begin
      raise ErbwParserError.CreateMode(Format(StrErrorAVariableNa, [NewName]), 1);
    end;
    AVariable := FVariables.Objects[Index] as TCustomValue;
    FVariables.Delete(Index);
    Index := FVariables.IndexOfObject(AVariable);
    while Index >= 0 do
    begin
      FVariables.Delete(Index);
      Index := FVariables.IndexOfObject(AVariable);
    end;
    AVariable.FName := NewName;
    AVariable.FUserName := NewUserName;
    Assert(AVariable is TCustomVariable);
    TCustomVariable(AVariable).DisplayName := NewDisplayName;

    Index := FVariables.AddObject(NewName, AVariable);
    FVariables.AddObject(NewDisplayName, AVariable);
    NewExpressions := TStringList.Create;
    try
      for ExpressionIndex := 0 to FExpressions.Count - 1 do
      begin
        AnExpression := FExpressions.Objects[ExpressionIndex] as TExpression;
        NewExpressions.AddObject(AnExpression.Decompile, AnExpression)
      end;
      NewExpressions.Duplicates := dupAccept;
      NewExpressions.CaseSensitive := True;
      NewExpressions.Sorted := True;
      FExpressions.Free;
      FExpressions := NewExpressions;
    except
      NewExpressions.Free;
      raise;
    end;

  end;

end;

procedure TRbwParser.DeleteExpression(const Index: integer);
begin
  Expressions[Index].Free;
  FExpressions.Delete(Index);
end;

procedure TRbwParser.RemoveExpression(const Expression: TExpression);
var
  Position: integer;
begin
  Position := FExpressions.IndexOfObject(Expression);
  if Position >= 0 then
  begin
    DeleteExpression(Position);
  end;
end;

procedure TRbwParser.RemoveVariable(const Variable: TCustomVariable);
var
  Index: integer;
  Expression: TExpression;
  Position: integer;
begin
  for Index := ExpressionCount - 1 downto 0 do
  begin
    Expression := Expressions[Index];
    if Expression.UsesVariable(Variable) then
    begin
      Expression.Free;
      FExpressions.Delete(Index);
    end;
  end;
  Position := FVariables.IndexOfObject(Variable);
  While Position >= 0 do
  begin
    FVariables.Delete(Position);
    Position := FVariables.IndexOfObject(Variable);
  end;
  FOwnedVariables.Remove(Variable)
end;

function TRbwParser.ExpressionCount: integer;
begin
  result := FExpressions.Count;
end;

{ TCustomValue }

constructor TCustomValue.Create(const VariableName: string;
  const DataType: TRbwDataType);
begin
  inherited Create(DataType);
  FUserName := Trim(VariableName);
  FName := FUserName;
end;

function TCustomValue.Decompile: string;
begin
  result := FUserName;
end;

function TCustomValue.DecompileDisplay: string;
begin
  result := FUserName;
end;

{ TRealVariable }

constructor TRealVariable.Create(const VariableName: string; const NameToDisplay: string);
begin
  inherited Create(VariableName, rdtDouble, NameToDisplay);
end;

{$WARNINGS OFF}
function TRealVariable.GetValue: double;
begin
  result := PDouble(FResult)^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TRealVariable.SetValue(const Value: double);
begin
  PDouble(FResult)^ := Value;
end;
{$WARNINGS ON}

{ TIntegerVariable }

constructor TIntegerVariable.Create(const VariableName: string; const NameToDisplay: string);
begin
  inherited Create(VariableName, rdtInteger, NameToDisplay);
end;

{$WARNINGS OFF}
function TIntegerVariable.GetValue: Integer;
begin
  result := PInteger(FResult)^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TIntegerVariable.SetValue(const Value: Integer);
begin
  PInteger(FResult)^ := Value;
end;
{$WARNINGS ON}

{ TBooleanVariable }

constructor TBooleanVariable.Create(const VariableName: string; const NameToDisplay: string);
begin
  inherited Create(VariableName, rdtBoolean, NameToDisplay);
end;

{$WARNINGS OFF}
function TBooleanVariable.GetValue: Boolean;
begin
  result := PBoolean(FResult)^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TBooleanVariable.SetValue(const Value: Boolean);
begin
  PBoolean(FResult)^ := Value;
end;
{$WARNINGS ON}

{ TStringVariable }

constructor TStringVariable.Create(const VariableName: string; const NameToDisplay: string);
begin
  inherited Create(VariableName, rdtString, NameToDisplay);
end;

function TStringVariable.GetValue: string;
begin
  result := ResultString;
end;

procedure TStringVariable.SetValue(const Value: string);
begin
  ResultString := Value;
end;

{ TConstant }

constructor TConstant.Create(Value: string);
begin
  Create(rdtString);
  if (Length(Value) > 0) and (Value[1] = '"') and (Value[Length(Value)] = '"')
    then
  begin
    Value := Copy(Value, 2, Length(Value) - 2);
  end;
  ResultString := Value;
end;

{$WARNINGS OFF}
constructor TConstant.Create(const DataType: TRbwDataType);
begin
  inherited Create;
  FResultType := DataType;
  case ResultType of
    rdtDouble:
      begin
        New(PDouble(FResult));
      end;
    rdtInteger:
      begin
        New(PInteger(FResult));
      end;
    rdtBoolean:
      begin
        New(PBoolean(FResult));
      end;
    rdtString:
      begin
        FResult := @ResultString;
      end;
  else
    Assert(False);
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
constructor TConstant.Create(const Value: Boolean);
begin
  Create(rdtBoolean);
  PBoolean(FResult)^ := Value;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TConstant.BooleanResult: boolean;
begin
  if ResultType <> rdtBoolean then
  begin
    raise ErbwParserError.Create(Format(StrErrorAttemptToUsB,
      [DataTypeToString(ResultType)]));
  end;
  result := PBoolean(FResult)^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
constructor TConstant.Create(const Value: double);
begin
  Create(rdtDouble);
  PDouble(FResult)^ := Value;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
constructor TConstant.Create(const Value: integer);
begin
  Create(rdtInteger);
  PInteger(FResult)^ := Value;
end;
{$WARNINGS ON}

destructor TConstant.Destroy;
begin
  if FResultType <> rdtString then
  begin
    case FResultType of
      rdtDouble:
        begin
          Dispose(PDouble(FResult));
        end;
      rdtInteger:
        begin
          Dispose(PInteger(FResult));
        end;
      rdtBoolean:
        begin
          Dispose(PBoolean(FResult));
        end;
    end;
  end;
  inherited;
end;

{$WARNINGS OFF}
function TConstant.DoubleResult: double;
begin
  if not (ResultType in [rdtDouble, rdtInteger]) then
  begin
    raise ErbwParserError.Create(Format(StrErrorAttemptToUsR,
      [DataTypeToString(ResultType)]));
  end;
  if ResultType = rdtInteger then
  begin
    result := PInteger(FResult)^;
  end
  else
  begin
    result := PDouble(FResult)^;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TConstant.IntegerResult: Integer;
begin
  if ResultType <> rdtInteger then
  begin
    raise ErbwParserError.Create(Format(StrErrorAttemptToUsI,
      [DataTypeToString(ResultType)]));
  end;
  result := PInteger(FResult)^;
end;
{$WARNINGS ON}

procedure TConstant.MakeDiagram(List: TStringList; Level: integer);
var
  Line: String;
  Index: Integer;
begin
  Line := '';
  for Index := 0 to Level - 1 do
  begin
    Line := Line + #9;
  end;
  Line := Line + DecompileDisplay + #9 + DecompileDisplay;
  List.Add(Line);
end;

function TConstant.StringResult: string;
begin
  if ResultType <> rdtString then
  begin
    raise ErbwParserError.Create(Format(StrErrorAttemptToUsS,
      [DataTypeToString(ResultType)]));
  end;
  result := ResultString;
end;

function TConstant.Decompile: string;
begin
  result := ValueToString;
end;

function TConstant.DecompileDisplay: string;
begin
  result := ValueToString;
end;

function TConstant.ValueToString: string;
//var
//  OldDecimalSeparator: Char;
begin
  result := '';
  case ResultType of
    rdtDouble:
      begin
        result := InternalFloatToStr(DoubleResult);
        if (Pos('.', result) <= 0) and (Pos('e', result) <= 0) and (Pos('E',
          result) <= 0) then
        begin
          result := result + '.';
        end;
      end;
    rdtInteger:
      begin
        result := IntToStr(IntegerResult);
      end;
    rdtBoolean:
      begin
        if BooleanResult then
        begin
          result := 'True';
        end
        else
        begin
          result := 'False';
        end;
      end;
    rdtString:
      begin
        result := '"' + StringResult + '"';
      end;
  else
    Assert(False);
  end;
end;

{$WARNINGS OFF}
procedure TConstant.SetResultString(const Value: string);
begin
  FResultString := Value;
  FResult := @FResultString
end;
{$WARNINGS ON}

{ TIntegerStack }

{$WARNINGS OFF}
procedure TIntegerStack.Clear;
var
  Index: integer;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    FreeMem(FList[Index]);
  end;
  FList.Clear;
end;
{$WARNINGS ON}

function TIntegerStack.Count: integer;
begin
  result := FList.Count;
end;

constructor TIntegerStack.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TIntegerStack.Destroy;
begin
  Clear;
  FList.Free;
  inherited;

end;

function TIntegerStack.GetCapacity: integer;
begin
  result := FList.Capacity;
end;

{$WARNINGS OFF}
function TIntegerStack.Peek: Integer;
begin
  result := PInteger(FList[FList.Count - 1])^
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TIntegerStack.Pop: integer;
var
  LastPointer: PInteger;
begin
  if FList.Count = 0 then
  begin
    raise ERangeError.Create(StrErrorTheStackIs);
  end;
  LastPointer := FList[FList.Count - 1];
  result := LastPointer^;
  FreeMem(LastPointer);
  FList.Count := FList.Count - 1;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TIntegerStack.Push(const AnInteger: integer);
var
  LastPointer: PInteger;
begin
  GetMem(LastPointer, SizeOf(Integer));
  LastPointer^ := AnInteger;
  FList.Add(LastPointer);
end;
{$WARNINGS ON}

procedure TIntegerStack.SetCapacity(const Value: integer);
begin
  if Value <= 0 then
  begin
    Clear;
  end
  else
  begin
    while Capacity < Value do
    begin
      Pop;
    end;
    FList.Capacity := Value;
  end;
end;

{ TTokenStringList }

function TTokenStringList.Add(const S: string): Integer;
var
  Token: string;
begin
  Token := FixToken(S);
  if (Token = '') then
  begin
    result := -1;
  end
  else
  begin
    result := inherited Add(Token);
  end;
end;

{$WARNINGS OFF}
procedure SetResultFromConstant(Constant: TConstant; Result: TExpression);
begin
  case Constant.FResultType of
    rdtDouble:
      begin
        PDouble(result.FResult)^ := PDouble(Constant.FResult)^;
      end;
    rdtInteger:
      begin
        PInteger(result.FResult)^ := PInteger(Constant.FResult)^;
      end;
    rdtBoolean:
      begin
        PBoolean(result.FResult)^ := PBoolean(Constant.FResult)^;
      end;
    rdtString:
      begin
        result.ResultString := Constant.ResultString;
      end;
  else
    Assert(False);
  end;
end;
{$WARNINGS ON}

function TTokenStringList.Compile(const Functions: TFunctionStringList;
  const Variables: TStringList;
  SpecialImplementorList: TSpecialImplementorList): TExpression;
var
  Index: integer;
  Token: string;
  ABoolean: Boolean;
  AnInteger: integer;
  ADouble: double;
  Position: integer;
  ParenStack: TIntegerStack;
  StartParenPosition: integer;
  PriorExpression: TExpression;
  VariableList: TList;
  VarIndex, VarIndex2: integer;
  AnObject: TObject;
  isVariable: boolean;
  Constant: TConstant;
  ResultConstant: TConstant;
  resultVariable: TCustomValue;
  procedure CleanUp;
  var
    Index: integer;
    VarIndex: integer;
  begin
    begin
      for Index := 0 to Count - 1 do
      begin
        AnObject := Objects[Index];
        if AnObject <> nil then
        begin
          if AnObject is TList then
          begin
            VariableList := AnObject as TList;
            for VarIndex := 0 to VariableList.Count - 1 do
            begin
              Constant := TConstant(VariableList[VarIndex]);
              if not (Constant is TCustomValue) or (Constant is TExpression)
                then
              begin
                Constant.Free;
              end;
            end;
            VariableList.Free;
            VariableList := nil;
          end
          else
          begin
            Constant := AnObject as TConstant;
            if not (Constant is TCustomValue) or (Constant is TExpression) then
            begin
              Constant.Free;
            end;
          end;
          Objects[Index] := nil;
        end;
      end;
      //      raise;
    end;
  end;
begin
  result := nil;
  try
    for Index := 0 to Count - 1 do
    begin
      Token := Strings[Index];
      if (Token = '(') or (Token = ')') or (Token = ',') then
      begin
        Continue;
      end
      else if IsStringConstant(Token) then
      begin
        Objects[Index] := TConstant.Create(Token);
      end
      else if IsBooleanConstant(Token, ABoolean) then
      begin
        Objects[Index] := TConstant.Create(ABoolean);
      end
      else if IsIntegerConstant(Token, AnInteger) then
      begin
        Objects[Index] := TConstant.Create(AnInteger);
      end
      else if IsFloatConstant(Token, ADouble) then
      begin
        Objects[Index] := TConstant.Create(ADouble);
      end
      {$IFDEF Delphi_2009_UP}
      else if (Length(Token) = 1) and CharInSet(Token[1], Parenthesis) then
      {$ELSE}
      else if (Length(Token) = 1) and (Token[1] in Parenthesis) then
      {$ENDIF}
      begin
        Continue;
      end
      else if (Variables <> nil) and (Variables.Find(Token, Position)) then
      begin
        Objects[Index] := Variables.Objects[Position];
      end
      else if (Functions <> nil) and (Functions.Find(Token, Position)) then
      begin
        Objects[Index] := TExpression.New(Functions.FunctionClass[Position],
          SpecialImplementorList);
      end;
    end;
    ParenStack := TIntegerStack.Create;
    try
      Index := 0;
      while Index < Count do
      begin
        Token := Strings[Index];
        if Token = '(' then
        begin
          ParenStack.Push(Index);
        end
        else if Token = ')' then
        begin
          StartParenPosition := ParenStack.Pop;
          MakeVariableList(SpecialImplementorList, StartParenPosition, Index);
          Index := StartParenPosition;
          isVariable := True;
          if Index > 0 then
          begin
            AnObject := Objects[Index - 1];
            if AnObject <> nil then
            begin
              PriorExpression := AnObject as TExpression;
              isVariable := False;
              VariableList := Objects[Index] as TList;
              if VariableList = nil then
              begin
                if ((Length(PriorExpression.Data)
                  - PriorExpression.FOptionalArguments) <> 0) then
                begin

                  if PriorExpression.FOptionalArguments = 0 then
                  begin
                    raise ErbwParserError.Create(Format(StrErrorThe0sFunc,
                      [PriorExpression.FName, Length(PriorExpression.Data)]));
                  end
                  else
                  begin
                    raise ErbwParserError.Create(Format(StrErrorThe0sFuncOpt,
                      [PriorExpression.FName,
                      Length(PriorExpression.Data) - PriorExpression.FOptionalArguments,
                      Length(PriorExpression.Data)]));
                  end;
                end;
              end
              else
              begin
                if (PriorExpression.FOptionalArguments < 0) then
                begin
                  if (Length(PriorExpression.Data)
                    - 1 > VariableList.Count) then
                  begin
                    raise ErbwParserError.Create(Format(StrErrorThe0sFunc2,
                      [PriorExpression.FName,
                      Length(PriorExpression.Data) - 1,
                      VariableList.Count]));
                  end;
                  PriorExpression.ResetDataLength(VariableList.Count);
                end
                else
                begin
                  if (Length(PriorExpression.Data) < VariableList.Count) then
                  begin
                    raise ErbwParserError.Create(Format(StrErrorTheSFuncti3,
                      [PriorExpression.FName,
                      Length(PriorExpression.Data),
                      VariableList.Count]));
                  end;
                  if (Length(PriorExpression.Data)
                    - PriorExpression.FOptionalArguments > VariableList.Count)
                      then
                  begin
                    raise ErbwParserError.Create(Format(StrErrorThe0sFunc4,
                      [PriorExpression.FName,
                      Length(PriorExpression.Data) -
                      PriorExpression.FOptionalArguments,
                      VariableList.Count]));
                  end;
                end;
                VarIndex2 := -1;
                try
                  for VarIndex := 0 to VariableList.Count - 1 do
                  begin
                    VarIndex2 := VarIndex;
                    PriorExpression.Variables[VarIndex] :=
                      TConstant(VariableList[VarIndex]);
                    VariableList[VarIndex] := nil;
                  end;
                except on E: ERbwParserError do
                  begin
                  {$IFDEF FPC}
                    raise ERbwParserError.Create(Format(StrErrorInArgumentNu,
                      [VarIndex2 + 1,
                      PriorExpression.Name,
                      Strings[Index],
                      E.Message]));
                  {$ELSE}
                    Exception.RaiseOuterException(ERbwParserError.Create(Format(StrErrorInArgumentNu,
                      [VarIndex2 + 1,
                      PriorExpression.Name,
                      Strings[Index],
                      E.Message])));
                  {$ENDIF}
                  end;
                end;
                VariableList.Free;
                VariableList := nil;
                Strings[Index - 1] := Strings[Index - 1] + Strings[Index];
                Delete(Index);
                Dec(Index);
              end;
            end;
          end;
          if isVariable then
          begin
            VariableList := Objects[Index] as TList;
            if (VariableList <> nil) then
            begin
              if (VariableList.Count > 1) then
              begin
                Dec(Index);
                MakeVariableList(SpecialImplementorList, Index, Index + 1);
                VariableList := Objects[Index] as TList;
                if VariableList.Count <> 1 then
                begin
                  for VarIndex := 0 to VariableList.Count - 1 do
                  begin
                    Constant := TConstant(VariableList[VarIndex]);
                    if not (Constant is TCustomValue) or (Constant is TExpression)
                      then
                    begin
                      Constant.Free;
                    end;
                  end;
                  VariableList.Free;
                  VariableList := nil;
                  Objects[Index] := nil;
                  raise ErbwParserError.Create(StrParsingErrorCheck);
                end;
              end;

              // Assert(VariableList.Count=1);
              Objects[Index] := TObject(VariableList[0]);
              VariableList.Free;
              VariableList := nil
            end;
          end;
        end;
        Inc(Index);
      end;
    finally
      ParenStack.Free;
    end;
    MakeVariableList(SpecialImplementorList, 0, Count - 1);
    VariableList := Objects[0] as TList;
    try
      if VariableList <> nil then
      begin
        if VariableList.Count <> 1 then
        begin
          for VarIndex := 0 to VariableList.Count - 1 do
          begin
            Constant := TConstant(VariableList[VarIndex]);
            if not (Constant is TCustomValue) or (Constant is TExpression) then
            begin
              Constant.Free;
            end;
          end;
          VariableList.Free;
          VariableList := nil;
          Objects[0] := nil;
          raise ErbwParserError.Create(StrParsingErrorCheck);
        end;
        ResultConstant := TConstant(VariableList[0]);
        if ResultConstant is TExpression then
        begin
          result := TExpression(VariableList[0]);
          result.FillVariables;
          //result.MakeLinkedList(nil);
          Constant := result.ConvertToConstant;
          result.FillVariables;
          if Constant <> nil then
          begin
            SetResultFromConstant(Constant, Result);
            result.FunctionAddr := nil;
            result.ShouldEvaluate := False;
            result.FUserName := Constant.Decompile;
            Constant.Free;
          end;
        end
        else if ResultConstant is TCustomValue then
        begin
          resultVariable := TCustomValue(VariableList[0]);
          result := TVariableExpression.Create(resultVariable, SpecialImplementorList);
        end
        else
        begin
          result := TExpression.Create('Dummy',
            ResultConstant.ResultType, SpecialImplementorList);
          SetResultFromConstant(ResultConstant, result);
          result.FUserName := ResultConstant.Decompile;
          ResultConstant.Free;
        end;
      end;
    finally
      VariableList.Free;
      Objects[0] := nil;
    end;
  except
    on E: ERbwParserError do
    begin
      CleanUp;
      raise ;
    end;
    on E: Exception do
    begin
      CleanUp;
    {$IFDEF FPC}
      raise ERbwParserError.Create(
        E.ClassName + ': ' + E.Message);
    {$ELSE}
      Exception.RaiseOuterException(ERbwParserError.Create(
        E.ClassName + ': ' + E.Message));
    {$ENDIF}
    end;
  end
end;

function TTokenStringList.FixToken(const Token: string): string;
begin
  result := Trim(Token);
  if (Length(result) > 1) and (Result[1] = '+') then
  begin
    result := Copy(result, 2, MAXINT);
  end;
end;

procedure TTokenStringList.Insert(Index: Integer; const S: string);
var
  Token: string;
begin
  Token := FixToken(S);
  if Token = '' then
  begin
    Exit;
  end
  else
  begin
    inherited Insert(Index, Token);
  end;
end;

procedure TTokenStringList.EvaluateSpecialImplementors(
  SpecialImplementorList: TSpecialImplementorList; Start: integer;
  var Stop: integer);
var
  Functions, Arguments: TList;
  Index: integer;
  AnExpression: TExpression;
  AnArgument: TConstant;
  AnObject: TObject;
  FunctionIndex: integer;
  ArgumentIndex: integer;
  Token: string;
  FunctionClass: TFunctionClass;
  DeleteFunction: boolean;
  LastArgument: integer;
  NewString: string;
  AlternateNameIndex: integer;
  function DataType: TRbwDataType;
  begin
    if ArgumentIndex >= FunctionClass.InputDataCount then
    begin
      Assert(FunctionClass.OptionalArguments <> 0);
      if FunctionClass.InputDataCount = 0 then
      begin
        result := FunctionClass.OptionalType;
      end
      else
      begin
        result := FunctionClass.InputDataTypes[FunctionClass.InputDataCount - 1];
      end;
    end
    else
    begin
      result := FunctionClass.InputDataTypes[ArgumentIndex];
    end;

  end;
  procedure FillArgumentList;
  var
    ArgumentIndex: integer;
    AList: TList;
    ListIndex: integer;
  begin
    Arguments.Clear;
    if (Index + 1 < Count) and (Strings[Index + 1][1] <> '(') then
    begin
      Exit;
    end;
    for ArgumentIndex := Index + 1 to Stop do
    begin
      LastArgument := ArgumentIndex;
      AnObject := Objects[ArgumentIndex];
      if AnObject is TList then
      begin
        AList := TList(AnObject);
        for ListIndex := 0 to AList.Count - 1 do
        begin
          Arguments.Add(AList[ListIndex]);
        end;
      end
      else
      begin
        AnArgument := AnObject as TConstant;
        if AnArgument <> nil then
        begin
          Arguments.Add(AnArgument);
        end;
      end;
      if Arguments.Count >= FunctionClass.InputDataCount then
      begin
        if Arguments.Count = 0 then
        begin
          Dec(LastArgument);
        end;
        break;
      end;
    end;
  end;
begin
  LastArgument := -1;
  Functions := TList.Create;
  Arguments := TList.Create;
  try
    Index := Start;
    while Index <= Stop do
    begin
      Token := UpperCase(Strings[Index]);
      AnObject := Objects[Index];
      if (AnObject = nil) and (Token <> '(')
        and (Token <> ')') and (Token <> ',') then
      begin
        Functions.Clear;
        for FunctionIndex := 0 to SpecialImplementorList.Count - 1 do
        begin
          FunctionClass := SpecialImplementorList[FunctionIndex].FunctionClass;
          if UpperCase(FunctionClass.Name) = Token then
          begin
            Functions.Add(FunctionClass);
          end
          else
          begin
            for AlternateNameIndex := 0 to
              FunctionClass.Synonyms.Count -1 do
            begin
              if UpperCase(FunctionClass.Synonyms[
                AlternateNameIndex]) = Token  then
              begin
                Functions.Add(FunctionClass);
                break;
              end;
            end;
          end;
        end;
        for FunctionIndex := Functions.Count - 1 downto 0 do
        begin
          FunctionClass := TFunctionClass(Functions[FunctionIndex]);
          FillArgumentList;
          if FunctionClass.OptionalArguments < 0 then
          begin
            if (Arguments.Count < FunctionClass.InputDataCount - 1) then
            begin
              Functions.Delete(FunctionIndex);
              Continue;
            end;
          end
          else
          begin
            if (Arguments.Count <
              FunctionClass.InputDataCount - FunctionClass.OptionalArguments) then
            begin
              Functions.Delete(FunctionIndex);
              Continue;
            end;
          end;
          DeleteFunction := False;
          for ArgumentIndex := 0 to Arguments.Count - 1 do
          begin
            AnArgument := TConstant(Arguments[ArgumentIndex]);
            if DataType <> AnArgument.ResultType then
            begin
              if (DataType = rdtDouble)
                and (AnArgument.ResultType = rdtInteger) then
              begin
                Continue;
              end
              else
              begin
                DeleteFunction := True;
                break;
              end;
            end;
          end;
          if DeleteFunction then
          begin
            Functions.Delete(FunctionIndex);
            Continue;
          end;
        end;
        if Functions.Count > 1 then
        begin
          for FunctionIndex := Functions.Count - 1 downto 0 do
          begin
            FunctionClass := TFunctionClass(Functions[FunctionIndex]);
            FillArgumentList;
            DeleteFunction := False;
            for ArgumentIndex := 0 to Arguments.Count - 1 do
            begin
              AnArgument := TConstant(Arguments[ArgumentIndex]);
              if DataType <> AnArgument.ResultType then
              begin
                DeleteFunction := True;
                break;
              end;
            end;
            if DeleteFunction then
            begin
              Functions.Delete(FunctionIndex);
              Continue;
            end;
          end;
        end;
        if Functions.Count = 0 then
        begin
          Inc(Index);
          Continue;
        end;
        Assert(Functions.Count = 1);
        FunctionClass := TFunctionClass(Functions[0]);
        AnExpression := TExpression.New(FunctionClass, SpecialImplementorList);
        if FunctionClass.OptionalArguments <> 0 then
        begin
          SetLength(AnExpression.Data, Arguments.Count);
          SetLength(AnExpression.VariablesForFunction, Arguments.Count);

        end;

        Objects[Index] := AnExpression;
        for ArgumentIndex := 0 to Arguments.Count - 1 do
        begin
          AnArgument := TConstant(Arguments[ArgumentIndex]);
          AnExpression.Data[ArgumentIndex].DataType := DataType;
          AnExpression.Variables[ArgumentIndex] := AnArgument;
        end;
        NewString := '';
        for ArgumentIndex := Index to Stop do
        begin
          NewString := NewString + Strings[ArgumentIndex];
          if (ArgumentIndex >= LastArgument)
            or (Strings[Index] = ')') or (Strings[Index] = ',') then
          begin
            LastArgument := ArgumentIndex;
            break;
          end;
        end;
        Strings[Index] := NewString;
        for ArgumentIndex := LastArgument downto Index + 1 do
        begin
          AnObject := Objects[ArgumentIndex];
          if AnObject is TList then
          begin
            AnObject.Free;
          end;
          Delete(ArgumentIndex);
        end;
        Stop := Stop - LastArgument + Index;

      end;
      Inc(Index);
    end;
  finally
    Functions.Free;
    Arguments.Free;
  end;
end;

function TTokenStringList.IsSign(Index: Integer; const Token: string): Boolean;
var
  AnArgument: TConstant;
begin
  AnArgument := Objects[Index + 1] as TConstant;
  result := False;
  if AnArgument <> nil then
  begin
    if ((AnArgument.ResultType = rdtInteger) or (AnArgument.ResultType = rdtDouble)) then
    begin
      // The next item is consistent with a sign operator.
      // Check the previous item.
      if (Index = 0) then
      begin
        result := True;
      end
      else
      begin
        if (Index < 0) then
        begin
          raise ErbwParserError.Create(Format(StrErrorInParsingS, [Token]));
        end;
        result := (Objects[Index - 1] = nil);
      end;
    end;
  end;
end;

procedure TTokenStringList.EvaluateUnaryOperator(
  SpecialImplementorList: TSpecialImplementorList;
  OperatorDefinition: TOperatorDefinition; var Stop: Integer; var Index: Integer);
var
  AnArgument: TConstant;
  UsedDef: TOperatorArgumentDefinition;
  DefIndex: Integer;
  ArgumentDef: TOperatorArgumentDefinition;
  AnExpression: TExpression;
begin
  if (Objects[Index] <> nil) or (Index + 1 >= Count) then
  begin
    raise ErbwParserError.Create(Format(StrErrorInParsingSOp,
      [string(OperatorDefinition.OperatorName)]));
  end;

  if not OperatorDefinition.SignOperator or
    IsSign(Index, string(OperatorDefinition.OperatorName)) then
  begin
    if (Objects[Index + 1] = nil) then
    begin
      raise ErbwParserError.Create(Format(StrErrorInParsing02,
        [string(OperatorDefinition.OperatorName), Strings[Index + 1]]));
    end;
    AnArgument := nil;
    try
      AnArgument := Objects[Index + 1] as TConstant;
    except on EInvalidCast do
    {$IFDEF FPC}
      raise ErbwParserError.Create(Format(StrErrorInParsing03,
        [string(OperatorDefinition.OperatorName),
        Strings[Index + 1]]));
    {$ELSE}
      Exception.RaiseOuterException(ErbwParserError.Create(Format(StrErrorInParsing03,
        [string(OperatorDefinition.OperatorName),
        Strings[Index + 1]])));
    {$ENDIF}
    end;
    if (OperatorDefinition.ArgumentDefinitions.Count < 1) then
    begin
      raise ErbwParserError.Create(Format(StrTheSOperatorMu,
        [string(OperatorDefinition.OperatorName)]));
    end;
    UsedDef := nil;
    for DefIndex := 0 to OperatorDefinition.ArgumentDefinitions.Count - 1 do
    begin
      ArgumentDef := OperatorDefinition.ArgumentDefinitions[DefIndex];
      if (AnArgument.ResultType = ArgumentDef.FirstArgumentType)
        {or ((ArgumentDef.FirstArgumentType = rdtDouble)
        and (AnArgument.ResultType = rdtInteger))} then
      begin
        UsedDef := ArgumentDef;
        break;
      end;
    end;
    if (UsedDef = nil) then
    begin
      raise ErbwParserError.Create(Format(StrErrorInParsing0,
        [string(OperatorDefinition.OperatorName),
        DataTypeToString(AnArgument.ResultType)]));
    end;
    Assert((AnArgument <> nil)
      and (AnArgument.ResultType = UsedDef.FirstArgumentType));
    AnExpression := nil;
    case UsedDef.CreationMethod of
      cmCreate:
        begin
          AnExpression := UsedDef.OperatorClass.Create(
            UsedDef.FunctionClass, SpecialImplementorList);
        end;
      cmNew:
        begin
          AnExpression := UsedDef.OperatorClass.New(
            UsedDef.FunctionClass, SpecialImplementorList);
        end;
      else Assert(False);
    end;
    //    AnExpression := TOperator.New(NotOperator, SpecialImplementorList);
    AnExpression.Variables[0] := AnArgument;
    Objects[Index] := AnExpression;
    Strings[Index] := Strings[Index] + ' ' + Strings[Index + 1];
    Delete(Index + 1);
    Dec(Stop);
  end;
end;

procedure TRbwParser.DefineNotOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OpDef: TOperatorDefinition;
begin
  OpDef := TOperatorDefinition.Create;
  OpDef.OperatorName := 'NOT';
  OpDef.ArgumentCount := acOne;
  OpDef.Precedence := p1;
  OpDef.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmNew;
  ArgumentDef.OperatorClass := TOperator;
  ArgumentDef.FunctionClass := NotOperator;

  AddOperator(OpDef);
end;

procedure TRbwParser.DefinePowerOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OpDef: TOperatorDefinition;
begin
  OpDef := TOperatorDefinition.Create;
  OpDef.OperatorName := '^';
  OpDef.ArgumentCount := acTwo;
  OpDef.Precedence := p1;
  OpDef.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TOperator;
  ArgumentDef.FunctionClass := PowerOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TOperator;
  ArgumentDef.FunctionClass := PowerOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TOperator;
  ArgumentDef.FunctionClass := PowerOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TOperator;
  ArgumentDef.FunctionClass := PowerOperator;

  AddOperator(OpDef);
end;

procedure TRbwParser.DefinePowerOperator2;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OpDef: TOperatorDefinition;
begin
  OpDef := TOperatorDefinition.Create;
  OpDef.OperatorName := '**';
  OpDef.ArgumentCount := acTwo;
  OpDef.Precedence := p1;
  OpDef.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TOperator;
  ArgumentDef.FunctionClass := PowerOperator2;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TOperator;
  ArgumentDef.FunctionClass := PowerOperator2;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TOperator;
  ArgumentDef.FunctionClass := PowerOperator2;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TOperator;
  ArgumentDef.FunctionClass := PowerOperator2;

  AddOperator(OpDef);
end;

procedure TRbwParser.DefinePlusSignOperator;
var
  OpDef: TOperatorDefinition;
  ArgumentDef: TOperatorArgumentDefinition;
begin
  OpDef := TOperatorDefinition.Create;
  OpDef.OperatorName := '+';
  OpDef.ArgumentCount := acOne;
  OpDef.Precedence := p2;
  OpDef.SignOperator := True;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TSignOperator;
  ArgumentDef.FunctionClass := PlusSignIOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TSignOperator;
  ArgumentDef.FunctionClass := PlusSignROperator;

  AddOperator(OpDef);
end;

procedure TRbwParser.DefineMinusSignOperator;
var
  OpDef: TOperatorDefinition;
  ArgumentDef: TOperatorArgumentDefinition;
begin
  OpDef := TOperatorDefinition.Create;
  OpDef.OperatorName := '-';
  OpDef.ArgumentCount := acOne;
  OpDef.Precedence := p2;
  OpDef.SignOperator := True;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TSignOperator;
  ArgumentDef.FunctionClass := MinusSignIOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OpDef.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.OperatorClass := TSignOperator;
  ArgumentDef.FunctionClass := MinusSignROperator;

  AddOperator(OpDef);
end;

procedure TTokenStringList.EvaluateBinaryOperator(
  SpecialImplementorList: TSpecialImplementorList;
  OperatorDefinition: TOperatorDefinition;
  var Stop: Integer; var Index: Integer);
var
  UsedDef: TOperatorArgumentDefinition;
  SubsequentArgument: TConstant;
  PriorArgument: TConstant;
  AnExpression: TExpression;
  DefIndex: Integer;
  ArgumentDef: TOperatorArgumentDefinition;
  PriorArgOK: Boolean;
  SubsequentArgOK: Boolean;
begin
  if (Objects[Index] <> nil) or (Index + 1 >= Count) or (Index - 1 < 0) then
  begin
    raise ErbwParserError.Create(Format(StrErrorInParsingThe,
      [string(OperatorDefinition.OperatorName)]));
  end;
  PriorArgument := Objects[Index - 1] as TConstant;
  SubsequentArgument := Objects[Index + 1] as TConstant;
  if ((PriorArgument = nil) or (SubsequentArgument = nil)) then
  begin
    if ((PriorArgument = nil) and (SubsequentArgument = nil)) then
    begin
      raise ErbwParserError.Create(Format(StrErrorInParsing04,
        [string(OperatorDefinition.OperatorName),
        Strings[Index - 1],
        Strings[Index + 1]]));
    end
    else if (PriorArgument = nil) then
    begin
      raise ErbwParserError.Create(Format(StrErrorInParsing05,
        [string(OperatorDefinition.OperatorName),
        Strings[Index - 1]]));
    end
    else
    begin
      raise ErbwParserError.Create(Format(StrErrorInParsing05,
        [string(OperatorDefinition.OperatorName),
        Strings[Index + 1]]));
    end;
  end;
  UsedDef := nil;
  Assert(OperatorDefinition.ArgumentDefinitions.Count >= 1);
  for DefIndex := 0 to OperatorDefinition.ArgumentDefinitions.Count - 1 do
  begin
    ArgumentDef := OperatorDefinition.ArgumentDefinitions[DefIndex];
    PriorArgOK := (ArgumentDef.FirstArgumentType = PriorArgument.ResultType)
      {or ((ArgumentDef.FirstArgumentType = rdtDouble)
      and (PriorArgument.ResultType = rdtInteger))};
    SubsequentArgOK := (ArgumentDef.SecondArgumentType = SubsequentArgument.ResultType)
      {or ((ArgumentDef.SecondArgumentType = rdtDouble)
      and (SubsequentArgument.ResultType = rdtInteger))};
    if PriorArgOK and SubsequentArgOK then
    begin
      UsedDef := ArgumentDef;
      break;
    end;
  end;
  if UsedDef = nil then
  begin
    raise ErbwParserError.Create(Format(StrErrorInParsing06,
      [string(OperatorDefinition.OperatorName),
      DataTypeToString(PriorArgument.ResultType),
      DataTypeToString(SubsequentArgument.ResultType)]));
  end;
  AnExpression := nil;
  case UsedDef.CreationMethod of
    cmCreate:
      begin
        AnExpression := UsedDef.OperatorClass.Create(UsedDef.FunctionClass,
          SpecialImplementorList);
      end;
    cmNew:
      begin
        AnExpression := UsedDef.OperatorClass.New(UsedDef.FunctionClass,
          SpecialImplementorList);
      end;
  else
    Assert(False);
  end;
  AnExpression.Variables[0] := PriorArgument;
  AnExpression.Variables[1] := SubsequentArgument;
  Objects[Index - 1] := AnExpression;
  Strings[Index - 1] := Strings[Index - 1] + ' ' + Strings[Index] + ' ' + Strings[Index + 1];
  Delete(Index + 1);
  Delete(Index);
  Dec(Stop, 2);
  Dec(Index);
end;

procedure TRbwParser.AddOperator(OpDef: TOperatorDefinition);
begin
  Assert(OpDef <> nil);
  Assert(OpDef.ArgumentDefinitions.Count > 0);
  {$IFDEF Delphi_2009_UP}
  OpDef.OperatorName := AnsiString(AnsiUpperCase(string(OpDef.OperatorName)));
  {$ELSE}
  OpDef.OperatorName := UpperCase(OpDef.OperatorName);
  {$ENDIF}
  FOpereratorDefinitions.Add(OpDef);
  if Length(OpDef.OperatorName) = 1 then
  begin
    if OpDef.SignOperator then
    begin
      Include(Signs, OpDef.OperatorName[1]);
    end
    else
    begin
      Operators.Add(string(OpDef.OperatorName));
    end;
  end
  else
  begin
    WordOperators.Add(string(OpDef.OperatorName));
  end;
end;

procedure TRbwParser.RemoveOperator(OperatorName: AnsiString);
var
  Index: Integer;
  OpDef: TOperatorDefinition;
  Position: Integer;
begin
  {$IFDEF Delphi_2009_UP}
  OperatorName := AnsiString(AnsiUpperCase(string(OperatorName)));
  {$ELSE}
  OperatorName := UpperCase(OperatorName);
  {$ENDIF}
  for Index := 0 to FOpereratorDefinitions.Count - 1 do
  begin
    OpDef := TOperatorDefinition(FOpereratorDefinitions[Index]);
    if OpDef.OperatorName = OperatorName then
    begin
      ClearExpressions;

      if Length(OpDef.OperatorName) = 1 then
      begin
        if OpDef.SignOperator then
        begin
          Exclude(Signs, OpDef.OperatorName[1]);
        end
        else
        begin
          Position := Operators.IndexOf(string(OpDef.OperatorName));
          if Position >= 0 then
          begin
            Operators.Delete(Position);
          end;
        end;
      end
      else
      begin
          Position := WordOperators.IndexOf(string(OpDef.OperatorName));
          if Position >= 0 then
          begin
            WordOperators.Delete(Position);
          end;
      end;

      FOpereratorDefinitions.Delete(Index);
      break;
    end;
  end;
end;

procedure TRbwParser.DefineTimesOperator;
var
  OperatorDefinition: TOperatorDefinition;
  ArgumentDef: TOperatorArgumentDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '*';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p3;
  OperatorDefinition.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := TimesROperator;
  ArgumentDef.OperatorClass := TOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := TimesROperator;
  ArgumentDef.OperatorClass := TOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := TimesROperator;
  ArgumentDef.OperatorClass := TOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := TimesIOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineDivideOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '/';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p3;
  OperatorDefinition.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := DivideROperator;
  ArgumentDef.OperatorClass := TOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := DivideROperator;
  ArgumentDef.OperatorClass := TOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := DivideROperator;
  ArgumentDef.OperatorClass := TOperator;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := DivideIOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineDivOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := 'DIV';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p3;
  OperatorDefinition.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := DivOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineModOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := 'MOD';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p3;
  OperatorDefinition.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := ModOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineAndOperator;
var
  OperatorDefinition: TOperatorDefinition;
  ArgumentDef: TOperatorArgumentDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := 'AND';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p3;
  OperatorDefinition.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmNew;
  ArgumentDef.FunctionClass := AndOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefinePlusOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '+';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p4;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := PlusROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := PlusROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := PlusROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := PlusIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtString;
  ArgumentDef.SecondArgumentType := rdtString;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := PlusSOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineMinusOperator;
var
  OperatorDefinition: TOperatorDefinition;
  ArgumentDef: TOperatorArgumentDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '-';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p4;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := MinusROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := MinusROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := MinusROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := MinusIOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineOrOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := 'OR';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p4;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmNew;
  ArgumentDef.FunctionClass := OrOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineXorOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := 'XOR';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p4;
  OperatorDefinition.SignOperator := False;

  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmNew;
  ArgumentDef.FunctionClass := XorOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineEqualsOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '=';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p5;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := EqualROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := EqualROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := EqualROperator;
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
  ArgumentDef.FirstArgumentType := rdtString;
  ArgumentDef.SecondArgumentType := rdtString;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := EqualSOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := EqualBOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineNotEqualsOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '<>';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p5;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := NotEqualROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := NotEqualROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := NotEqualROperator;
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
  ArgumentDef.FirstArgumentType := rdtString;
  ArgumentDef.SecondArgumentType := rdtString;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := NotEqualSOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := NotEqualBOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineLessThanOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '<';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p5;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtString;
  ArgumentDef.SecondArgumentType := rdtString;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanSOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanBOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineGreaterThanOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '>';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p5;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtString;
  ArgumentDef.SecondArgumentType := rdtString;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanSOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanBOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TRbwParser.DefineLessThanOrEqualsOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '<=';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p5;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanOrEqualsROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanOrEqualsROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanOrEqualsROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanOrEqualsIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtString;
  ArgumentDef.SecondArgumentType := rdtString;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanOrEqualsSOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := LessThanOrEqualsBOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;


procedure TRbwParser.DefineGreaterThanOrEqualsOperator;
var
  ArgumentDef: TOperatorArgumentDefinition;
  OperatorDefinition: TOperatorDefinition;
begin
  OperatorDefinition := TOperatorDefinition.Create;
  OperatorDefinition.OperatorName := '>=';
  OperatorDefinition.ArgumentCount := acTwo;
  OperatorDefinition.Precedence := p5;
  OperatorDefinition.SignOperator := False;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanOrEqualsROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtDouble;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanOrEqualsROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtDouble;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanOrEqualsROperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtInteger;
  ArgumentDef.SecondArgumentType := rdtInteger;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanOrEqualsIOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtString;
  ArgumentDef.SecondArgumentType := rdtString;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanOrEqualsSOperator;
  ArgumentDef.OperatorClass := TOperator;
  //
  ArgumentDef := TOperatorArgumentDefinition.Create;
  OperatorDefinition.ArgumentDefinitions.Add(ArgumentDef);
  ArgumentDef.FirstArgumentType := rdtBoolean;
  ArgumentDef.SecondArgumentType := rdtBoolean;
  ArgumentDef.CreationMethod := cmCreate;
  ArgumentDef.FunctionClass := GreaterThanOrEqualsBOperator;
  ArgumentDef.OperatorClass := TOperator;

  AddOperator(OperatorDefinition);
end;

procedure TTokenStringList.EvaluateOverloadedFunctions(
  SpecialImplementorList: TSpecialImplementorList; Start: integer;
  var Stop: integer);
var
  Functions, Arguments: TList;
  Index: integer;
  AnExpression: TExpression;
  AnArgument: TConstant;
  AnObject: TObject;
  FunctionIndex: integer;
  ArgumentIndex: integer;
  Token: string;
  FunctionClass: TFunctionClass;
  DeleteFunction: boolean;
  LastArgument: integer;
  NewString: string;
  AlternateNameIndex: integer;
  function DataType: TRbwDataType;
  begin
    if ArgumentIndex >= FunctionClass.InputDataCount then
    begin
      Assert(FunctionClass.OptionalArguments <> 0);
      result := FunctionClass.InputDataTypes[FunctionClass.InputDataCount - 1];
    end
    else
    begin
      result := FunctionClass.InputDataTypes[ArgumentIndex];
    end;

  end;
  procedure FillArgumentList;
  var
    ArgumentIndex: integer;
    AList: TList;
    ListIndex: integer;
  begin
    Arguments.Clear;
    for ArgumentIndex := Index + 1 to Stop do
    begin
      LastArgument := ArgumentIndex;
      AnObject := Objects[ArgumentIndex];
      if AnObject is TList then
      begin
        AList := TList(AnObject);
        for ListIndex := 0 to AList.Count - 1 do
        begin
          Arguments.Add(AList[ListIndex]);
        end;
      end
      else
      begin
        AnArgument := AnObject as TConstant;
        if AnArgument <> nil then
        begin
          Arguments.Add(AnArgument);
        end;
      end;
      if Arguments.Count >= FunctionClass.InputDataCount then
      begin
        break;
      end;
    end;
  end;
begin
  Functions := TList.Create;
  Arguments := TList.Create;
  try
    Index := Start;
    while Index <= Stop do
    begin
      Token := UpperCase(Strings[Index]);
      AnObject := Objects[Index];
      if (AnObject = nil) and (Token <> '(')
        and (Token <> ')') and (Token <> ',') then
      begin
        Functions.Clear;
        for FunctionIndex := 0 to OverloadedFunctionList.Count - 1 do
        begin
          FunctionClass := OverloadedFunctionList[FunctionIndex]
            as TFunctionClass;
          if UpperCase(FunctionClass.Name) = Token then
          begin
            Functions.Add(FunctionClass);
          end
          else
          begin
            for AlternateNameIndex := 0 to
              FunctionClass.Synonyms.Count -1 do
            begin
              if UpperCase(FunctionClass.Synonyms[
                AlternateNameIndex]) = Token then
              begin
                Functions.Add(FunctionClass);
                break;
              end;
            end;
          end;
        end;
        for FunctionIndex := Functions.Count - 1 downto 0 do
        begin
          FunctionClass := TFunctionClass(Functions[FunctionIndex]);
          FillArgumentList;
          if FunctionClass.OptionalArguments < 0 then
          begin
            if (Arguments.Count < FunctionClass.InputDataCount - 1) then
            begin
              Functions.Delete(FunctionIndex);
              Continue;
            end;
          end
          else
          begin
            if (Arguments.Count < FunctionClass.InputDataCount) then
            begin
              Functions.Delete(FunctionIndex);
              Continue;
            end;
          end;
          DeleteFunction := False;
          for ArgumentIndex := 0 to Arguments.Count - 1 do
          begin
            AnArgument := TConstant(Arguments[ArgumentIndex]);
            if DataType <> AnArgument.ResultType then
            begin
              if (DataType = rdtDouble)
                and (AnArgument.ResultType = rdtInteger) then
              begin
                Continue;
              end
              else
              begin
                DeleteFunction := True;
                break;
              end;
            end;
          end;
          if DeleteFunction then
          begin
            Functions.Delete(FunctionIndex);
            Continue;
          end;
        end;
        if Functions.Count > 1 then
        begin
          for FunctionIndex := Functions.Count - 1 downto 0 do
          begin
            FunctionClass := TFunctionClass(Functions[FunctionIndex]);
            FillArgumentList;
            DeleteFunction := False;
            for ArgumentIndex := 0 to Arguments.Count - 1 do
            begin
              AnArgument := TConstant(Arguments[ArgumentIndex]);
              if DataType <> AnArgument.ResultType then
              begin
                DeleteFunction := True;
                break;
              end;
            end;
            if DeleteFunction then
            begin
              Functions.Delete(FunctionIndex);
              Continue;
            end;
          end;
        end;
        if Functions.Count = 0 then
        begin
          Inc(Index);
          Continue;
        end;
        Assert(Functions.Count = 1);
        FunctionClass := TFunctionClass(Functions[0]);
        AnExpression := TExpression.New(FunctionClass, SpecialImplementorList);
        if FunctionClass.OptionalArguments <> 0 then
        begin
          SetLength(AnExpression.Data, Arguments.Count);
          SetLength(AnExpression.VariablesForFunction, Arguments.Count);
        end;

        Objects[Index] := AnExpression;
        for ArgumentIndex := 0 to Arguments.Count - 1 do
        begin
          AnArgument := TConstant(Arguments[ArgumentIndex]);
          AnExpression.Data[ArgumentIndex].DataType := DataType;
          AnExpression.Variables[ArgumentIndex] := AnArgument;
        end;
        NewString := '';
        for ArgumentIndex := Index to Stop do
        begin
          NewString := NewString + Strings[ArgumentIndex];
          if (ArgumentIndex >= LastArgument)
            or (Strings[Index] = ')') or (Strings[Index] = ',') then
          begin
            LastArgument := ArgumentIndex;
            break;
          end;
        end;
        Strings[Index] := NewString;
        for ArgumentIndex := LastArgument downto Index + 1 do
        begin
          AnObject := Objects[ArgumentIndex];
          if AnObject is TList then
          begin
            AnObject.Free;
          end;
          Delete(ArgumentIndex);
        end;
        Stop := Stop - LastArgument + Index;

      end;
      Inc(Index);
    end;
  finally
    Functions.Free;
    Arguments.Free;
  end;
end;

procedure TTokenStringList.EvaluateOperators(
  SpecialImplementorList: TSpecialImplementorList; Start: integer;
  var Stop: integer);
var
  Index: integer;
  Token: string;
  PrecedenceIndex: TPrecedence;
  DefIndex: Integer;
  OpDef: TOperatorDefinition;
begin
  for PrecedenceIndex := Low(TPrecedence) to High(TPrecedence) do
  begin
    Index := Start;
    while Index <= Stop do
    begin
      Token := Uppercase(Strings[Index]);
      for DefIndex := 0 to FOpereratorDefinitions.Count - 1 do
      begin
        OpDef := TOperatorDefinition(FOpereratorDefinitions[DefIndex]);
        if OpDef.Precedence = PrecedenceIndex then
        begin
          if Token = string(OpDef.OperatorName) then
          begin
            case OpDef.ArgumentCount of
              acOne:
                begin
                  EvaluateUnaryOperator(SpecialImplementorList, OpDef, Stop, Index);
                end;
              acTwo:
                begin
                  EvaluateBinaryOperator(SpecialImplementorList, OpDef, Stop, Index);
                end;
              else Assert(False);
            end;
          end;
        end;
      end;
      Inc(Index);
    end;
  end;
end;

procedure TTokenStringList.MakeVariableList(
  SpecialImplementorList: TSpecialImplementorList; Start, Stop: integer);
var
  ExpressionString: string;
  Index: integer;
  ResultList: TList;
  AnObject: TObject;
  Token: string;
begin
  if Start < 0 then
  begin
    raise ERbwParserError.Create('Unable to evaluate formula');
  end;
  EvaluateSpecialImplementors(SpecialImplementorList, Start, Stop);
  EvaluateOverloadedFunctions(SpecialImplementorList, Start, Stop);

  EvaluateOperators(SpecialImplementorList, Start, Stop);
//  EvaluateNotAndSigns(SpecialImplementorList, Start, Stop);
//  EvaluateMultDivEtc(SpecialImplementorList, Start, Stop);
//  EvaluatePlusMinusOrXor(SpecialImplementorList, Start, Stop);
//  EvaluateBoolOperators(SpecialImplementorList, Start, Stop);

  ResultList := TList.Create;
  try
    for Index := Start to Stop do
    begin
      AnObject := Objects[Index];
      if AnObject = nil then
      begin
        Token := Strings[Index];
        if (Token <> '(') and (Token <> ',') and (Token <> ')') then
        begin
          raise ErbwParserError.Create(Format(StrError0sNotRe, [Token]));
        end;
      end
      else
      begin
        ResultList.Add(AnObject);
      end;
    end;
    ExpressionString := '';
    for Index := Start to Stop do
    begin
      ExpressionString := ExpressionString + Strings[Index];
    end;
    Strings[Start] := ExpressionString;
    if ResultList.Count = 0 then
    begin
      ResultList.Free;
      ResultList := nil;
    end
    else
    begin
      Objects[Start] := ResultList;
      ResultList := nil;
    end;
    for Index := Stop downto Start + 1 do
    begin
      Delete(Index);
    end;
  except
    ResultList.Free;
    raise;
  end;
end;

{ TExpression }

procedure TExpression.Initalize(const FunctionAddress: Pointer;
  const DataTypes: array of TRbwDataType; const OptionalArguments: integer);
var
  Index: Integer;
begin
  FunctionAddr := FunctionAddress;
  ShouldEvaluate := Assigned(FunctionAddr);
  SetLength(Data, Length(DataTypes));
  SetLength(VariablesForFunction, Length(DataTypes));
  for Index := 0 to Length(DataTypes) - 1 do
  begin
    with Data[Index] do
    begin
      DataType := DataTypes[Index];
      Datum := nil;
    end;
  end;
  FOptionalArguments := OptionalArguments;
end;

procedure TExpression.ResetDataLength(const Count: integer);
var
  ArrayLength: integer;
  LastDataType: TRbwDataType;
  Index: integer;
begin
  Assert(FOptionalArguments < 0);
  ArrayLength := Length(Data);
  Assert(ArrayLength > 0);
  LastDataType := Data[ArrayLength - 1].DataType;
  SetLength(Data, Count);
  for Index := ArrayLength to Count - 1 do
  begin
    with Data[Index] do
    begin
      DataType := LastDataType;
      Datum := nil;
    end;
  end;
  SetLength(VariablesForFunction, Count);
end;

procedure TExpression.Evaluate;
var
  Index: integer;
  AVariable: TConstant;
  I: integer;
begin
  if ShouldEvaluate then
  begin
    for Index := 0 to Length(Data) - 1 do
    begin
      if Data[Index].Datum <> nil then
      begin
        AVariable := TConstant(Data[Index].Datum);
        if AVariable is TExpression then
        begin
          TExpression(AVariable).Evaluate;
        end;
      end;
    end;
    for Index := 0 to StringVariableCount - 1 do
    begin
      I := StringVariableIndicies[Index];
      AVariable := TConstant(Data[I].Datum);
      VariablesForFunction[I] := AVariable.FResult;
    end;
    SetResultFromFunction;
  end;
end;


constructor TExpression.Create(const FunctionClass: TFunctionClass; SpecialImplementorList: TSpecialImplementorList);
begin
  if FunctionClass.FunctionRecord.Prototype = '' then
  begin
    FunctionClass.FunctionRecord.Prototype := FunctionClass.Prototype;
  end;
  Create(FunctionClass.FunctionRecord, SpecialImplementorList);
end;

function TExpression.GetVariables(const Index: integer): TConstant;
begin
  result := TConstant(Data[Index].Datum);
end;

procedure TExpression.SetVariables(const Index: integer;
  const Value: TConstant);
var
  Converter: TExpression;
begin
  if ((Value = nil) or (Data[Index].DataType = Value.ResultType)
    or ((Data[Index].DataType = rdtDouble) and (Value.ResultType = rdtInteger)))
      then
  begin
    if (Data[Index].DataType = rdtDouble) and (Value.ResultType = rdtInteger)
      then
    begin
      Converter := TExpression.New(IntToDoubleFunction, FSpecialImplementorList);
      Converter.Variables[0] := Value;
      Data[Index].Datum := Converter;
    end
    else
    begin
      Data[Index].Datum := Value;
    end;
  end
  else
  begin
    raise ErbwParserError.Create(Format(StrError0sCanNot,
      [DataTypeToString(Value.ResultType),
      DataTypeToString(Data[Index].DataType)]));
  end;

end;

destructor TExpression.Destroy;
var
  Index: integer;
  AConstant: TConstant;
begin
  FNotifier.Free;
  for Index := 0 to Length(Data) - 1 do
  begin
    AConstant := Variables[Index];
    if (AConstant is TExpression) or not (AConstant is TCustomValue) then
    begin
      AConstant.Free;
    end;
  end;
  FVariablesUsed.Free;
  inherited;
end;

procedure TExpression.Diagram(List: TStringList);
begin
  List.Clear;
  MakeDiagram(List, 0);
end;

constructor TExpression.Create(const FunctionRecord: TFunctionRecord; SpecialImplementorList: TSpecialImplementorList);
var
  LinePosition: integer;
begin
  Create(FunctionRecord.Name, FunctionRecord.ResultType,
    FunctionRecord.CanConvertToConstant, SpecialImplementorList);
  FPrototype := FunctionRecord.Prototype;
  LinePosition := Pos('|', FPrototype);
  while LinePosition >= 1 do
  begin
    FPrototype := Copy(FPrototype, LinePosition+1, MAXINT);
    LinePosition := Pos('|', FPrototype);
  end;
  Initalize(@FunctionRecord.RFunctionAddr, FunctionRecord.InputDataTypes,
    FunctionRecord.OptionalArguments);
end;

function TExpression.ConvertToConstant: TConstant;
var
  Index: Integer;
  AConstant: TConstant;
  NewConstant: TConstant;
  CanConvert: boolean;
begin
  ShouldEvaluate := Assigned(FunctionAddr);
  result := nil;
  if not AllowConversionToConstant then
    Exit;
  CanConvert := True;
  for Index := 0 to Length(Data) - 1 do
  begin
    AConstant := Variables[Index];
    if AConstant <> nil then
    begin
      if AConstant is TExpression then
      begin
        NewConstant := TExpression(AConstant).ConvertToConstant;
        if NewConstant = nil then
        begin
          CanConvert := False;
        end
        else
        begin
          AConstant.Free;
          Variables[Index] := NewConstant;
        end;
      end
      else if AConstant is TCustomValue then
      begin
        CanConvert := False;
      end;
    end;
  end;
  if CanConvert then
  begin
    FillVariables;
    Evaluate;
    case ResultType of
      rdtDouble:
        begin
          result := TConstant.Create(DoubleResult);
        end;
      rdtInteger:
        begin
          result := TConstant.Create(IntegerResult);
        end;
      rdtBoolean:
        begin
          result := TConstant.Create(BooleanResult);
        end;
      rdtString:
        begin
          result := TConstant.Create(StringResult);
        end;
    else
      Assert(False);
    end;
  end;
  ShouldEvaluate := Assigned(FunctionAddr);
end;

{$WARNINGS OFF}
function _Not(Values: array of pointer): Boolean;
begin
  result := not PBoolean(Values[0])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Xor(Values: array of pointer): Boolean;
begin
  result := PBoolean(Values[0])^ xor PBoolean(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _TimesI(Values: array of pointer): Integer;
begin
  result := PInteger(Values[0])^ * PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _TimesR(Values: array of pointer): double;
begin
  result := PDouble(Values[0])^ * PDouble(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _DivideR(Values: array of pointer): double;
begin
  result := PDouble(Values[0])^ / PDouble(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _DivideI(Values: array of pointer): double;
begin
  result := PInteger(Values[0])^ / PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Div(Values: array of pointer): Integer;
begin
  result := PInteger(Values[0])^ div PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Mod(Values: array of pointer): Integer;
begin
  result := PInteger(Values[0])^ mod PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _And(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[0])^ and PBoolean(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Or(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[0])^ or PBoolean(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _PlusSignI(Values: array of pointer): integer;
begin
  result := PInteger(Values[0])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _PlusSignR(Values: array of pointer): double;
begin
  result := PDouble(Values[0])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _MinusSignI(Values: array of pointer): integer;
begin
  result := -PInteger(Values[0])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _MinusSignR(Values: array of pointer): double;
begin
  result := -PDouble(Values[0])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _PlusI(Values: array of pointer): integer;
begin
  result := PInteger(Values[0])^ + PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _PlusR(Values: array of pointer): double;
begin
  result := PDouble(Values[0])^ + PDouble(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _PlusS(Values: array of pointer): string;
var
  String1, String2: string;
begin
  String1 := PString(Values[0])^;
  String2 := PString(Values[1])^;
  result := String1 + String2;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _MinusI(Values: array of pointer): integer;
begin
  result := PInteger(Values[0])^ - PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _MinusR(Values: array of pointer): double;
begin
  result := PDouble(Values[0])^ - PDouble(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _EqualR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ = PDouble(Values[1])^;
end;
{$WARNINGS OFF}

{$WARNINGS OFF}
function _EqualI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ = PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _EqualS(Values: array of pointer): boolean;
var
  String1, String2: String;
begin
  String1 := PString(Values[0])^;
  String2 := PString(Values[1])^;
  result := String1 = String2;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _EqualB(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[0])^ = PBoolean(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _NotEqualB(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[0])^ <> PBoolean(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _NotEqualI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ <> PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _NotEqualR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ <> PDouble(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _NotEqualS(Values: array of pointer): boolean;
var
  String1, String2: string;
begin
  String1 := PString(Values[0])^;
  String2 := PString(Values[1])^;
  result := String1 <> String2;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _LessThanB(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[0])^ < PBoolean(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _LessThanI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ < PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _LessThanR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ < PDouble(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _LessThanS(Values: array of pointer): boolean;
var
  String1, String2: String;
begin
  String1 := PString(Values[0])^;
  String2 := PString(Values[1])^;
  result := AnsiCompareStr(String1, String2) < 0;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _GreaterThanB(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[0])^ > PBoolean(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _GreaterThanI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ > PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _GreaterThanR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ > PDouble(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _GreaterThanS(Values: array of pointer): boolean;
var
  String1, String2: String;
begin
  String1 := PString(Values[0])^;
  String2 := PString(Values[1])^;
  result := AnsiCompareStr(String1, String2) > 0;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _LessThanOrEqualsB(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[0])^ <= PBoolean(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _LessThanOrEqualsI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ <= PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _LessThanOrEqualsR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ <= PDouble(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _LessThanOrEqualsS(Values: array of pointer): boolean;
var
  String1, String2: string;
begin
  String1 := PString(Values[0])^;
  String2 := PString(Values[1])^;
  result := AnsiCompareStr(String1, String2) <= 0;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _GreaterThanOrEqualsB(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[0])^ >= PBoolean(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _GreaterThanOrEqualsI(Values: array of pointer): boolean;
begin
  result := PInteger(Values[0])^ >= PInteger(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _GreaterThanOrEqualsR(Values: array of pointer): boolean;
begin
  result := PDouble(Values[0])^ >= PDouble(Values[1])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _GreaterThanOrEqualsS(Values: array of pointer): boolean;
var
  String1, String2: String;
begin
  String1 := PString(Values[0])^;
  String2 := PString(Values[1])^;
  result := AnsiCompareStr(String1, String2) >= 0;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _IntToDouble(Values: array of pointer): double;
begin
  result := PInteger(Values[0])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _AbsR(Values: array of pointer): double;
begin
  result := Abs(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _AbsI(Values: array of pointer): integer;
begin
  result := Abs(PInteger(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Arccos(Values: array of pointer): double;
begin
  result := ArcCos(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Arccosh(Values: array of pointer): double;
begin
  result := ArcCosh(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Arcsin(Values: array of pointer): double;
begin
  result := ArcSin(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Arcsinh(Values: array of pointer): double;
begin
  result := ArcSinh(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Arctan(Values: array of pointer): double;
begin
  result := ArcTan(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Arctan2(Values: array of pointer): double;
begin
  result := ArcTan2(PDouble(Values[0])^, PDouble(Values[1])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Arctanh(Values: array of pointer): double;
begin
  result := ArcTanh(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Cos(Values: array of pointer): double;
begin
  result := Cos(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Cosh(Values: array of pointer): double;
begin
  result := Cosh(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Copy(Values: array of pointer): string;
begin
  result := Copy(PString(Values[0])^, PInteger(Values[1])^,
    PInteger(Values[2])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _DegToRad(Values: array of pointer): double;
begin
  result := DegToRad(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Exp(Values: array of pointer): double;
begin
  result := Exp(PDouble(Values[0])^)
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Factorial(Values: array of pointer): integer;
var
  StartValue: integer;
begin
  StartValue := PInteger(Values[0])^;
  if StartValue <= 1 then
  begin
    result := 1;
  end
  else if StartValue > 12 then
  begin
    raise EIntOverflow.Create(StrFactorialForValues);
  end
  else
  begin
    result := StartValue;
    while (StartValue > 1) do
    begin
      dec(StartValue);
      result := result * StartValue;
    end;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _FactorialR(Values: array of pointer): double;
var
  StartValue: integer;
begin
  StartValue := PInteger(Values[0])^;
  if StartValue <= 1 then
  begin
    result := 1;
  end
  else if StartValue > 170 then
  begin
    raise EOverflow.Create(StrFactorialForValues170);
  end
  else
  begin
    result := StartValue;
    while (StartValue > 1) do
    begin
      dec(StartValue);
      result := result * StartValue;
    end;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Frac(Values: array of pointer): double;
begin
  result := Frac(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _FloatToStr(Values: array of pointer): string;
//var
//  OldDecimalSeparator: Char;
begin
//  OldDecimalSeparator := DecimalSeparator;
//  try
//    DecimalSeparator := '.';
    result := InternalFloatToStr(PDouble(Values[0])^);
//  finally
//    DecimalSeparator := OldDecimalSeparator;
//  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _CaseBoolean(Values: array of pointer): boolean;
begin
  result := PBoolean(Values[PInteger(Values[0])^])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _PositionInList(Values: array of pointer): integer;
var
  TestItem: string;
  Index: Integer;
begin
  result := 0;
  TestItem := PString(Values[0])^;
  for Index := 1 to Length(Values) - 1 do
  begin
    if TestItem = PString(Values[Index])^ then
    begin
      result := Index;
      Exit;
    end;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _CaseInteger(Values: array of pointer): integer;
begin
  result := PInteger(Values[PInteger(Values[0])^])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _CaseDouble(Values: array of pointer): double;
begin
  result := PDouble(Values[PInteger(Values[0])^])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Closest(Values: array of pointer): integer;
var
  TestValue: double;
  AValue: double;
  Delta: double;
  TestDelta: double;
  index: integer;
begin
  TestValue := PDouble(Values[0])^;
  AValue := PDouble(Values[1])^;
  Delta := Abs(AValue-TestValue);
  result := 1;
  for index := 2 to Length(Values) - 1 do
  begin
    AValue := PDouble(Values[index])^;
    TestDelta := Abs(AValue-TestValue);
    if TestDelta < Delta then
    begin
      result := index;
      Delta := TestDelta;
    end;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _CaseString(Values: array of pointer): string;
begin
  result := PString(Values[PInteger(Values[0])^])^;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _IfBoolean(Values: array of pointer): boolean;
begin
  if PBoolean(Values[0])^ then
  begin
    result := PBoolean(Values[1])^;
  end
  else
  begin
    result := PBoolean(Values[2])^;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _IfInteger(Values: array of pointer): integer;
begin
  if PBoolean(Values[0])^ then
  begin
    result := PInteger(Values[1])^;
  end
  else
  begin
    result := PInteger(Values[2])^;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _IfDouble(Values: array of pointer): double;
begin
  if PBoolean(Values[0])^ then
  begin
    result := PDouble(Values[1])^;
  end
  else
  begin
    result := PDouble(Values[2])^;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _IfString(Values: array of pointer): string;
begin
  if PBoolean(Values[0])^ then
  begin
    result := PString(Values[1])^;
  end
  else
  begin
    result := PString(Values[2])^;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _IntPower(Values: array of pointer): double;
begin
  result := IntPower(PDouble(Values[0])^, PInteger(Values[1])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _IntToStr(Values: array of pointer): string;
begin
  result := IntToStr(PInteger(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Length(Values: array of pointer): integer;
begin
  result := Length(PString(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _ln(Values: array of pointer): double;
begin
  result := ln(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _log10(Values: array of pointer): double;
begin
  result := log10(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _logN(Values: array of pointer): double;
begin
  result := logN(PDouble(Values[0])^, PDouble(Values[1])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _LowerCase(Values: array of pointer): string;
begin
  result := LowerCase(PString(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _MaxI(Values: array of pointer): integer;
var
  Index: integer;
begin
  result := Max(PInteger(Values[0])^, PInteger(Values[1])^);
  for Index := 2 to Length(Values) - 1 do
  begin
    result := Max(result, PInteger(Values[Index])^);
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _MaxR(Values: array of pointer): double;
var
  Index: integer;
begin
  result := Max(PDouble(Values[0])^, PDouble(Values[1])^);
  for Index := 2 to Length(Values) - 1 do
  begin
    result := Max(result, PDouble(Values[Index])^);
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _MinI(Values: array of pointer): integer;
var
  Index: integer;
begin
  result := Min(PInteger(Values[0])^, PInteger(Values[1])^);
  for Index := 2 to Length(Values) - 1 do
  begin
    result := Min(result, PInteger(Values[Index])^);
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _MinR(Values: array of pointer): double;
var
  Index: integer;
begin
  result := Min(PDouble(Values[0])^, PDouble(Values[1])^);
  for Index := 2 to Length(Values) - 1 do
  begin
    result := Min(result, PDouble(Values[Index])^);
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _MultiInterpolate(Values: array of pointer): double;
begin
  result := 0;
  Assert(False);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Odd(Values: array of pointer): boolean;
begin
  result := Odd(PInteger(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Pos(Values: array of pointer): integer;
begin
  result := Pos(PString(Values[0])^, PString(Values[1])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _PosEx(Values: array of pointer): integer;
begin
  result := PosEx(PString(Values[0])^, PString(Values[1])^,
    PInteger(Values[2])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Power(Values: array of pointer): double;
begin
  result := Power(PDouble(Values[0])^, PDouble(Values[1])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Pi(Values: array of pointer): double;
begin
  result := Pi;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _RadToDeg(Values: array of pointer): double;
begin
  result := RadToDeg(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Round(Values: array of pointer): integer;
var
  AValue: double;
begin
  AValue := PDouble(Values[0])^;
  if AValue > MaxInt then
  begin
    result := MaxInt;
  end
  else if AValue < -MaxInt then
  begin
    result := -MaxInt;
  end
  else
  begin
    result := Round(AValue);
  end;

end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Sin(Values: array of pointer): double;
begin
  result := Sin(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Sinh(Values: array of pointer): double;
begin
  result := Sinh(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _SqrR(Values: array of pointer): double;
begin
  result := sqr(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _SqrI(Values: array of pointer): integer;
begin
  result := sqr(PInteger(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Sqrt(Values: array of pointer): double;
begin
  result := sqrt(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _StrToInt(Values: array of pointer): integer;
begin
  result := StrToInt(PString(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _StrToIntDef(Values: array of pointer): integer;
begin
  result := StrToIntDef(PString(Values[0])^, PInteger(Values[1])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _StrToFloat(Values: array of pointer): double;
var
  AString: string;
  Code: Integer;
begin
  AString := PString(Values[0])^;
  Val(AString, result, Code);
  if Code <> 0 then
  begin
    raise EConvertError.Create(Format(StrSCanNotBeConver, [AString]));
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _StrToFloatDef(Values: array of pointer): double;
var
  AString: string;
  Code: Integer;
begin
  AString := PString(Values[0])^;
  Val(AString, result, Code);
  if Code <> 0 then
  begin
    result := PDouble(Values[1])^;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Tan(Values: array of pointer): double;
begin
  result := tan(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Tanh(Values: array of pointer): double;
begin
  result := tanh(PDouble(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Trim(Values: array of pointer): string;
begin
  result := Trim(PString(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Trunc(Values: array of pointer): integer;
var
  AValue: double;
begin
  AValue := PDouble(Values[0])^;
  if AValue > MaxInt then
  begin
    result := MaxInt;
  end
  else if AValue < -MaxInt then
  begin
    result := -MaxInt;
  end
  else
  begin
    result := trunc(AValue);
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _UpperCase(Values: array of pointer): string;
begin
  result := UpperCase(PString(Values[0])^);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Interpolate(Values: array of pointer): double;
var
  Position: double;
  Value1: double;
  Distance1: double;
  Value2: double;
  Distance2: double;
begin
  Position := PDouble(Values[0])^;
  Value1 := PDouble(Values[1])^;
  Distance1 := PDouble(Values[2])^;
  Value2 := PDouble(Values[3])^;
  Distance2 := PDouble(Values[4])^;
  result := (Position - Distance1) / (Distance2 - Distance1)
    * (Value2 - Value1) + Value1;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function _Distance(Values: array of pointer): double;
var
  X1: double;
  Y1: double;
  X2: double;
  Y2: double;
begin
  X1 := PDouble(Values[0])^;
  Y1 := PDouble(Values[1])^;
  X2 := PDouble(Values[2])^;
  Y2 := PDouble(Values[3])^;
  result := Sqrt(Sqr(X1 - X2) + Sqr(Y1 - Y2));
end;
{$WARNINGS ON}

class function TExpression.New(
  const FunctionClass: TFunctionClass;
  SpecialImplementorList: TSpecialImplementorList): TExpression;
var
  Index: integer;
  SpecialImplementor: TSpecialImplementor;
begin
  if (FunctionClass = IntToDoubleFunction)
    or (FunctionClass.FunctionRecord.Name = IntToDoubleFunction.Name) then
  begin
    result := TIntToDoubleExpression.Create(FunctionClass, SpecialImplementorList);
  end
  else if FunctionClass = AndOperator then
  begin
    result := TAndExpression.Create(FunctionClass, SpecialImplementorList);
  end
  else if FunctionClass = OrOperator then
  begin
    result := TOrExpression.Create(FunctionClass, SpecialImplementorList);
  end
  else if FunctionClass = XorOperator then
  begin
    result := TOperator.Create(FunctionClass, SpecialImplementorList);
  end
  else if (FunctionClass = CaseBOverloadedFunction)
    or (FunctionClass = CaseIOverloadedFunction)
    or (FunctionClass = CaseROverloadedFunction)
    or (FunctionClass = CaseSOverloadedFunction)
    or (FunctionClass = IfBOverloadedFunction)
    or (FunctionClass = IfIOverloadedFunction)
    or (FunctionClass = IfROverloadedFunction)
    or (FunctionClass = IfSOverloadedFunction)
    or (FunctionClass.FunctionRecord.Name = IfBooleanFunction.Name)
    or (FunctionClass.FunctionRecord.Name = IfIntegerFunction.Name)
    or (FunctionClass.FunctionRecord.Name = IfRealFunction.Name)
    or (FunctionClass.FunctionRecord.Name = IfStringFunction.Name)
    or (FunctionClass.FunctionRecord.Name = CaseBooleanFunction.Name)
    or (FunctionClass.FunctionRecord.Name = CaseIntegerFunction.Name)
    or (FunctionClass.FunctionRecord.Name = CaseDoubleFunction.Name)
    or (FunctionClass.FunctionRecord.Name = CaseStringFunction.Name) then
  begin
    result := TSelectExpression.Create(FunctionClass, SpecialImplementorList);
  end
  else if (FunctionClass = MultiInterpolateFunction) then
  begin
    result := TMultiInterpolateExpression.Create(FunctionClass, SpecialImplementorList);
  end
  else
  begin
    for Index := 0 to SpecialImplementorList.Count - 1 do
    begin
      SpecialImplementor := SpecialImplementorList[Index];
      if (SpecialImplementor.FunctionClass = FunctionClass) then
      begin
        result := SpecialImplementor.Implementor.Create(
          SpecialImplementor.FunctionClass, SpecialImplementorList);
        Exit;
      end;
    end;
    result := TExpression.Create(FunctionClass, SpecialImplementorList);
  end;
end;

constructor TExpression.Create(const VariableName: string;
  const DataType: TRbwDataType; const CanConvertToConstant: boolean;
  SpecialImplementorList: TSpecialImplementorList);
begin
  Create(VariableName, DataType, SpecialImplementorList);
  FunctionAddr := nil;
  ShouldEvaluate := False;
  AllowConversionToConstant := CanConvertToConstant;
end;

procedure TExpression.SetAllowConversionToConstant(const Value: boolean);
begin
  FAllowConversionToConstant := Value;
end;

procedure TExpression.MakeDiagram(List: TStringList; Level: integer);
var
  Line: String;
  Index: Integer;
  ArrayLength: integer;
  AVariable: TConstant;
  Prototype: string;
begin
  ArrayLength := Length(Data);
  if (FProtoType = '') or (ArrayLength = 0) or (FProtoType = Name) then
  begin
    Prototype := '';
  end
  else
  begin
    Prototype := '  :: ' + FProtoType
  end;
  Line := '';
  for Index := 0 to Level - 1 do
  begin
    Line := Line + #9;
  end;

  Line := Line + FUserName  + Prototype + #9 + DecompileDisplay + Prototype;
  List.Add(Line);

  for Index := 0 to ArrayLength - 1 do
  begin
    if Data[Index].Datum <> nil then
    begin
      AVariable := TConstant(Data[Index].Datum);
      AVariable.MakeDiagram(List, Level+1);
    end;
  end;

end;

{$IFDEF Delphi_2009_UP}
function ConcatenateStrings(const List: TStrings): string;
var
  AStringBuilder: TStringBuilder;
  index: Integer;
begin
  AStringBuilder := TStringBuilder.Create;
  try
    for index := 0 to List.Count - 1 do
    begin
      AStringBuilder.Append(List[index])
    end;
    result := AStringBuilder.ToString;
  finally
    AStringBuilder.Free;
  end;
end;
{$ELSE}
function ConcatenateStrings(const List: TStrings): string;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S: string;
  NilIndex: integer;
begin
  Count := List.Count;
  Size := 0;
  for I := 0 to Count - 1 do
  begin
    Inc(Size, Length(List[I]));
  end;
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := List[I];
    NilIndex := Pos(Char(0),S);
    while NilIndex > 0 do
    begin
      S[NilIndex] := ' ';
      NilIndex := Pos(Char(0),S);
    end;
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L*SizeOf(Char));
      Inc(P, L);
    end;
//    P^ := ', ';
//    Inc(P, 2);
  end;
end;
{$ENDIF}

function TExpression.UsesFunction(FunctionName: string): boolean;
var
  ArrayLength: Integer;
  Index: Integer;
  AVariable: TConstant;
begin
  result := SameText(FunctionName, FName);
  if not result then
  begin
    ArrayLength := Length(Data);
    if (ArrayLength > 0) and Assigned(FunctionAddr) then
    begin
      for Index := 0 to ArrayLength - 1 do
      begin
        if Data[Index].Datum <> nil then
        begin
          AVariable := TConstant(Data[Index].Datum);
          if AVariable is TExpression then
          begin
            result := TExpression(AVariable).UsesFunction(FunctionName);
            if result then
            begin
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{$WARNINGS OFF}
procedure TExpression.SetResultFromFunction;
begin
  case ResultType of
    rdtDouble:
      begin
        PDouble(FResult)^ := TRbwRealFunction(FunctionAddr)(VariablesForFunction);
      end;
    rdtInteger:
      begin
        PInteger(FResult)^ := TRbwIntegerFunction(FunctionAddr)(VariablesForFunction);
      end;
    rdtBoolean:
      begin
        PBoolean(FResult)^ := TRbwBooleanFunction(FunctionAddr)(VariablesForFunction);
      end;
    rdtString:
      begin
        ResultString := TRbwStringFunction(FunctionAddr)(VariablesForFunction);
      end;
  else
    Assert(False);
  end;
end;
{$WARNINGS ON}

function TExpression.Decompile: string;
begin
  result := DecompileByType(dtInternal);
end;

function TExpression.DecompileByType(DecompileType: TDecompileType): string;
var
  Index: integer;
  ArrayLength: integer;
  AVariable: TConstant;
  NeedsParenthesis: boolean;
  ParametersPresent: boolean;
  DecompileList: TStringList;
begin
  result := FUserName;
  ArrayLength := Length(Data);
  if (ArrayLength > 0) and Assigned(FunctionAddr) then
  begin
    DecompileList := TStringList.Create;
    try
      DecompileList.Capacity := ArrayLength*2 + 2;
      DecompileList.Add(result);
      NeedsParenthesis := True;
      if ArrayLength = 1 then
      begin
        AVariable := TConstant(Data[0].Datum);
        if AVariable is TOperator then
        begin
          if Length(TOperator(AVariable).Data) = 2 then
          begin
            NeedsParenthesis := False;
          end;
        end;
      end;
      ParametersPresent := False;
      for Index := 0 to ArrayLength - 1 do
      begin
        if Data[Index].Datum <> nil then
        begin
          ParametersPresent := True;
          Break;
        end;
      end;

      if NeedsParenthesis and ParametersPresent then
      begin
        DecompileList.Add('(');
      end;
      for Index := 0 to ArrayLength - 1 do
      begin
        if Data[Index].Datum <> nil then
        begin
          if Index > 0 then
          begin
            DecompileList.Add(', ');
          end;
          AVariable := TConstant(Data[Index].Datum);
          if AVariable is TExpression then
          begin
            TExpression(AVariable).FTopLevel := False;
          end;

          case DecompileType of
            dtInternal: DecompileList.Add(AVariable.Decompile);
            dtDisplay: DecompileList.Add(AVariable.DecompileDisplay);
            else Assert(False);
          end;

        end;
      end;
      if NeedsParenthesis and ParametersPresent then
      begin
        DecompileList.Add(')');
      end;
      result := ConcatenateStrings(DecompileList);
    finally
      DecompileList.Free;
    end;
  end;
end;

function TExpression.DecompileDisplay: string;
begin
  result := DecompileByType(dtDisplay);
end;

function TExpression.UsesVariable(const Variable: TCustomVariable): boolean;
var
  Index: integer;
  ArrayLength: integer;
  SubVariable: TConstant;
begin
  Assert(Variable <> nil);
  result := False;
  ArrayLength := Length(Data);
  if ArrayLength > 0 then
  begin
    for Index := 0 to ArrayLength - 1 do
    begin
      result := TCustomVariable(Data[Index].Datum) = Variable;
      if result then
      begin
        Exit;
      end;
      if (Data[Index].Datum <> nil) then
      begin
        SubVariable := TConstant(Data[Index].Datum);
        if SubVariable is TExpression then
        begin
          result := TExpression(SubVariable).UsesVariable(Variable);
          if result then
          begin
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

procedure TExpression.FillVariables;
var
  Index: integer;
  AVariable: TConstant;
begin
  StringVariableCount := 0;
  Assert(Length(VariablesForFunction) = Length(Data));
  SetLength(StringVariableIndicies, Length(Data));

  for Index := 0 to Length(Data) - 1 do
  begin
    if Data[Index].Datum = nil then
    begin
      VariablesForFunction[Index] := nil;
    end
    else
    begin
      AVariable := TConstant(Data[Index].Datum);
      VariablesForFunction[Index] := AVariable.FResult;
      if (AVariable.FResultType = rdtString)
        and (AVariable is TCustomValue) then
      begin
        StringVariableIndicies[StringVariableCount] := Index;
        Inc(StringVariableCount);
      end;
      if AVariable is TExpression then
      begin
        TExpression(AVariable).FillVariables;
      end;
    end;
  end;
  SetLength(StringVariableIndicies, StringVariableCount);
end;

function TExpression.GetVariablesUsed: TStringList;
var
  Index: integer;
  ArrayLength: integer;
  AVariable: TConstant;
  InnerIndex: integer;
  TempList: TStringList;
begin
  FVariablesUsed.Clear;
  result := FVariablesUsed;
  ArrayLength := Length(Data);
  if (ArrayLength > 0) {and Assigned(FunctionAddr)} then
  begin
    for Index := 0 to ArrayLength - 1 do
    begin
      if Data[Index].Datum <> nil then
      begin
        AVariable := TConstant(Data[Index].Datum);
        if AVariable is TExpression then
        begin
          TempList := TExpression(AVariable).VariablesUsed;
          for InnerIndex := 0 to TempList.Count - 1 do
          begin
            result.AddObject(TempList[InnerIndex], TempList.Objects[InnerIndex]);
          end;
        end
        else if AVariable is TCustomValue then
        begin
          result.AddObject(AVariable.Decompile, AVariable);
        end;
      end;
    end;
  end;
end;

constructor TExpression.Create(const VariableName: string;
  const DataType: TRbwDataType; SpecialImplementorList: TSpecialImplementorList);
begin
  inherited Create(VariableName, DataType);
  FNotifier := TNotifierComponent.Create(self);
  FSpecialImplementorList := SpecialImplementorList;
  FVariablesUsed := TStringList.Create;
  FVariablesUsed.Sorted := True;
  FVariablesUsed.Duplicates := dupIgnore;
  FVariablesUsed.CaseSensitive := False;
  FTopLevel := True;
end;

{ TFunctionClass }

constructor TFunctionClass.Create;
begin
  FSynonyms := TStringList.Create;
  AllowConversionToConstant := True;
end;

destructor TFunctionClass.Destroy;
begin
  FSynonyms.Free;
  inherited;
end;

function TFunctionClass.GetAllowConversionToConstant: boolean;
begin
  result := FunctionRecord.CanConvertToConstant;
end;

function TFunctionClass.GetBFunctionAddr: TrbwBooleanFunction;
begin
  if FunctionRecord.ResultType <> rdtBoolean then
  begin
    raise ErbwParserError.Create(StrErrorTheFunction);
  end;
  result := FunctionRecord.BFunctionAddr;
end;

function TFunctionClass.GetHidden: boolean;
begin
  result := FunctionRecord.Hidden;
end;

function TFunctionClass.GetIFunctionAddr: TrbwIntegerFunction;
begin
  if FunctionRecord.ResultType <> rdtInteger then
  begin
    raise ErbwParserError.Create(StrErrorTheFunction);
  end;
  result := FunctionRecord.IFunctionAddr;
end;

function TFunctionClass.GetInputDataCount: integer;
begin
  result := Length(FunctionRecord.InputDataTypes);
end;

function TFunctionClass.GetInputDataTypes(const Index: integer): TRbwDataType;
begin
  if (Index < 0) or (Index >= InputDataCount) then
  begin
    raise ERangeError.Create(Format(StrDIsOutOfRange, [Index]));
  end;
  result := FunctionRecord.InputDataTypes[Index];
end;

function TFunctionClass.GetName: string;
begin
  result := FunctionRecord.Name
end;

function TFunctionClass.GetOptionalArguments: integer;
begin
  result := FunctionRecord.OptionalArguments;
end;

function TFunctionClass.GetOptionalType: TRbwDataType;
begin
  result := FunctionRecord.OptionalType;
end;

function TFunctionClass.GetResultType: TRbwDataType;
begin
  result := FunctionRecord.ResultType;
end;

function TFunctionClass.GetRFunctionAddr: TrbwRealFunction;
begin
  if FunctionRecord.ResultType <> rdtDouble then
  begin
    raise ErbwParserError.Create(StrErrorTheFunction);
  end;
  result := FunctionRecord.RFunctionAddr;
end;

function TFunctionClass.GetSFunctionAddr: TrbwStringFunction;
begin
  if FunctionRecord.ResultType <> rdtString then
  begin
    raise ErbwParserError.Create(StrErrorTheFunction);
  end;
  result := FunctionRecord.SFunctionAddr;
end;

procedure TFunctionClass.SetAllowConversionToConstant(
  const Value: boolean);
begin
  FunctionRecord.CanConvertToConstant := Value;
end;

procedure TFunctionClass.SetSynonyms(const Value: TStrings);
begin
  FSynonyms.Assign(Value);
end;

procedure TFunctionClass.SetBFunctionAddr(
  const Value: TrbwBooleanFunction);
begin
  FunctionRecord.BFunctionAddr := Value;
  FunctionRecord.ResultType := rdtBoolean;
end;

procedure TFunctionClass.SetHidden(const Value: boolean);
begin
  FunctionRecord.Hidden := Value
end;

procedure TFunctionClass.SetIFunctionAddr(
  const Value: TrbwIntegerFunction);
begin
  FunctionRecord.IFunctionAddr := Value;
  FunctionRecord.ResultType := rdtInteger;
end;

procedure TFunctionClass.SetInputDataCount(const Value: integer);
begin
  SetLength(FunctionRecord.InputDataTypes, Value);
end;

procedure TFunctionClass.SetInputDataTypes(const Index: integer;
  const Value: TRbwDataType);
begin
  if (Index < 0) or (Index >= InputDataCount) then
  begin
    raise ERangeError.Create(Format(StrDIsOutOfRange, [Index]));
  end;
  FunctionRecord.InputDataTypes[Index] := Value;
end;

procedure TFunctionClass.SetName(const Value: string);
begin
  FunctionRecord.Name := Value;
end;

procedure TFunctionClass.SetOptionalArguments(const Value: integer);
begin
  FunctionRecord.OptionalArguments := Value;
end;

procedure TFunctionClass.SetOptionalType(const Value: TRbwDataType);
begin
  FunctionRecord.OptionalType := Value;
end;

procedure TFunctionClass.SetRFunctionAddr(const Value: TrbwRealFunction);
begin
  FunctionRecord.RFunctionAddr := Value;
  FunctionRecord.ResultType := rdtDouble;
end;

procedure TFunctionClass.SetSFunctionAddr(const Value: TrbwStringFunction);
begin
  FunctionRecord.SFunctionAddr := Value;
  FunctionRecord.ResultType := rdtString;
end;

{ TFunctionStringList }

function TFunctionStringList.Add(
  const FunctionRecord: TFunctionRecord): Integer;
var
  AFunctionClass: TFunctionClass;
  IIndex: integer;
  ArrayLength: integer;
  AlternateNameIndex: integer;
begin
  AFunctionClass := TFunctionClass.Create;
  try
    ArrayLength := Length(FunctionRecord.InputDataTypes);
    SetLength(AFunctionClass.FunctionRecord.InputDataTypes, ArrayLength);
    for IIndex := 0 to ArrayLength - 1 do
    begin
      AFunctionClass.FunctionRecord.InputDataTypes[IIndex] :=
        FunctionRecord.InputDataTypes[IIndex];
    end;
    AFunctionClass.FunctionRecord.Synonyms := FunctionRecord.Synonyms;
    SetLength(AFunctionClass.FunctionRecord.Synonyms,
      Length(AFunctionClass.FunctionRecord.Synonyms));
    AFunctionClass.Hidden := FunctionRecord.Hidden;
    AFunctionClass.Prototype := FunctionRecord.Prototype;
    AFunctionClass.FunctionRecord.ResultType := FunctionRecord.ResultType;
    AFunctionClass.FunctionRecord.Name := FunctionRecord.Name;
    AFunctionClass.FunctionRecord.Prototype := FunctionRecord.Prototype;
    AFunctionClass.FunctionRecord.OptionalArguments :=
      FunctionRecord.OptionalArguments;
    AFunctionClass.FunctionRecord.CanConvertToConstant :=
      FunctionRecord.CanConvertToConstant;
    AFunctionClass.FunctionRecord.RFunctionAddr := FunctionRecord.RFunctionAddr;
    for AlternateNameIndex := 0 to Length(FunctionRecord.Synonyms) -1 do
    begin
      AFunctionClass.Synonyms.Add(FunctionRecord.
        Synonyms[AlternateNameIndex]);
    end;

    Assert(Assigned(AFunctionClass.FunctionRecord.RFunctionAddr));
    result := AddObject(FunctionRecord.Name, AFunctionClass);
    for IIndex := 0 to Length(AFunctionClass.FunctionRecord.Synonyms) -1 do
    begin
      AddObject(AFunctionClass.FunctionRecord.Synonyms[IIndex], AFunctionClass);
    end;

  except on EStringListError do
    begin
      AFunctionClass.Free;
      raise ErbwParserError.Create(Format(StrErrorAFunctionNa,
        [FunctionRecord.Name]));
    end;
  end;
end;

procedure TFunctionStringList.Clear;
var
  Index: integer;
  AFunctionClass: TFunctionClass;
begin
  // delete synonyms
  for Index := Count - 1 downto 0 do
  begin
    AFunctionClass := Objects[Index] as TFunctionClass;
    if AFunctionClass.FunctionRecord.Name <> Strings[Index] then
    begin
      inherited Delete(Index);
    end;
  end;

  // Free all non-synonyms.
  for Index := 0 to Count - 1 do
  begin
    AFunctionClass := Objects[Index] as TFunctionClass;
    AFunctionClass.Free;
  end;
  inherited;
end;

resourcestring
  StrMath = 'Math|';
  StrLogical = 'Logical|';
  StrTrig = 'Trig|';
  StrText = 'Text|';

constructor TFunctionStringList.Create;
begin
  inherited;
  CaseSensitive := False;
  Duplicates := dupError;
  Sorted := True;

  AbsFunctionI.ResultType := rdtInteger;
  AbsFunctionI.Name := 'AbsI';
  AbsFunctionI.Prototype := StrMath+'AbsI(Value)';
  SetLength(AbsFunctionI.InputDataTypes, 1);
  AbsFunctionI.InputDataTypes[0] := rdtInteger;
  AbsFunctionI.OptionalArguments := 0;
  AbsFunctionI.CanConvertToConstant := True;
  AbsFunctionI.IFunctionAddr := @_AbsI;
  Add(AbsFunctionI);

  AbsFunctionR.ResultType := rdtDouble;
  AbsFunctionR.Name := 'AbsR';
  AbsFunctionR.Prototype := StrMath+'AbsR(Value)';
  SetLength(AbsFunctionR.InputDataTypes, 1);
  AbsFunctionR.InputDataTypes[0] := rdtDouble;
  AbsFunctionR.OptionalArguments := 0;
  AbsFunctionR.CanConvertToConstant := True;
  AbsFunctionR.RFunctionAddr := @_AbsR;
  Add(AbsFunctionR);

  ArcCosFunction.ResultType := rdtDouble;
  ArcCosFunction.Name := 'ArcCos';
  ArcCosFunction.Prototype := StrTrig+'ArcCos(Value)';
  SetLength(ArcCosFunction.InputDataTypes, 1);
  ArcCosFunction.InputDataTypes[0] := rdtDouble;
  ArcCosFunction.OptionalArguments := 0;
  ArcCosFunction.CanConvertToConstant := True;
  ArcCosFunction.RFunctionAddr := @_arccos;
  Add(ArcCosFunction);

  ArcCoshFunction.ResultType := rdtDouble;
  ArcCoshFunction.Name := 'ArcCosh';
  ArcCoshFunction.Prototype := StrTrig+'ArcCosh(Value)';
  SetLength(ArcCoshFunction.InputDataTypes, 1);
  ArcCoshFunction.InputDataTypes[0] := rdtDouble;
  ArcCoshFunction.OptionalArguments := 0;
  ArcCoshFunction.CanConvertToConstant := True;
  ArcCoshFunction.RFunctionAddr := @_arccosh;
  Add(ArcCoshFunction);

  ArcSinFunction.ResultType := rdtDouble;
  ArcSinFunction.Name := 'ArcSin';
  ArcSinFunction.Prototype := StrTrig+'ArcSin(Value)';
  SetLength(ArcSinFunction.InputDataTypes, 1);
  ArcSinFunction.InputDataTypes[0] := rdtDouble;
  ArcSinFunction.OptionalArguments := 0;
  ArcSinFunction.CanConvertToConstant := True;
  ArcSinFunction.RFunctionAddr := @_arcsin;
  Add(ArcSinFunction);

  ArcSinhFunction.ResultType := rdtDouble;
  ArcSinhFunction.Name := 'ArcSinh';
  ArcSinhFunction.Prototype := StrTrig+'ArcSinh(Value)';
  SetLength(ArcSinhFunction.InputDataTypes, 1);
  ArcSinhFunction.InputDataTypes[0] := rdtDouble;
  ArcSinhFunction.OptionalArguments := 0;
  ArcSinhFunction.CanConvertToConstant := True;
  ArcSinhFunction.RFunctionAddr := @_arcsinh;
  Add(ArcSinhFunction);

  ArcTanFunction.ResultType := rdtDouble;
  ArcTanFunction.Name := 'ArcTan';
  ArcTanFunction.Prototype := StrTrig+'ArcTan(Value)';
  SetLength(ArcTanFunction.InputDataTypes, 1);
  ArcTanFunction.InputDataTypes[0] := rdtDouble;
  ArcTanFunction.OptionalArguments := 0;
  ArcTanFunction.CanConvertToConstant := True;
  ArcTanFunction.RFunctionAddr := @_arctan;
  Add(ArcTanFunction);

  ArcTan2Function.ResultType := rdtDouble;
  ArcTan2Function.Name := 'ArcTan2';
  ArcTan2Function.Prototype := StrTrig+'ArcTan2(Y, X)';
  SetLength(ArcTan2Function.InputDataTypes, 2);
  ArcTan2Function.InputDataTypes[0] := rdtDouble;
  ArcTan2Function.InputDataTypes[1] := rdtDouble;
  ArcTan2Function.OptionalArguments := 0;
  ArcTan2Function.CanConvertToConstant := True;
  ArcTan2Function.RFunctionAddr := @_arctan2;
  Add(ArcTan2Function);

  ArcTanhFunction.ResultType := rdtDouble;
  ArcTanhFunction.Name := 'ArcTanh';
  ArcTanhFunction.Prototype := StrTrig+'ArcTanh(Value)';
  SetLength(ArcTanhFunction.InputDataTypes, 1);
  ArcTanhFunction.InputDataTypes[0] := rdtDouble;
  ArcTanhFunction.OptionalArguments := 0;
  ArcTanhFunction.CanConvertToConstant := True;
  ArcTanhFunction.RFunctionAddr := @_arctanh;
  Add(ArcTanhFunction);

  CopyFunction.ResultType := rdtString;
  CopyFunction.Name := 'Copy';
  CopyFunction.Prototype := StrText+'Copy(Text_Value, StartIndex, Count)';
  SetLength(CopyFunction.InputDataTypes, 3);
  CopyFunction.InputDataTypes[0] := rdtString;
  CopyFunction.InputDataTypes[1] := rdtInteger;
  CopyFunction.InputDataTypes[2] := rdtInteger;
  CopyFunction.OptionalArguments := 0;
  CopyFunction.CanConvertToConstant := True;
  CopyFunction.SFunctionAddr := @_Copy;
  Add(CopyFunction);

  CosFunction.ResultType := rdtDouble;
  CosFunction.Name := 'Cos';
  CosFunction.Prototype := StrTrig+'Cos(Value)';
  SetLength(CosFunction.InputDataTypes, 1);
  CosFunction.InputDataTypes[0] := rdtDouble;
  CosFunction.OptionalArguments := 0;
  CosFunction.CanConvertToConstant := True;
  CosFunction.RFunctionAddr := @_cos;
  Add(CosFunction);

  CoshFunction.ResultType := rdtDouble;
  CoshFunction.Name := 'Cosh';
  CoshFunction.Prototype := StrTrig+'Cosh(Value)';
  SetLength(CoshFunction.InputDataTypes, 1);
  CoshFunction.InputDataTypes[0] := rdtDouble;
  CoshFunction.OptionalArguments := 0;
  CoshFunction.CanConvertToConstant := True;
  CoshFunction.RFunctionAddr := @_cosh;
  Add(CoshFunction);

  CaseBooleanFunction.ResultType := rdtBoolean;
  CaseBooleanFunction.Name := 'CaseB';
  CaseBooleanFunction.Prototype :=
    StrLogical+'CaseB(Index, Boolean_Result1, Boolean_Result2, ...)';
  SetLength(CaseBooleanFunction.InputDataTypes, 4);
  CaseBooleanFunction.InputDataTypes[0] := rdtInteger;
  CaseBooleanFunction.InputDataTypes[1] := rdtBoolean;
  CaseBooleanFunction.InputDataTypes[2] := rdtBoolean;
  CaseBooleanFunction.InputDataTypes[3] := rdtBoolean;
  CaseBooleanFunction.CanConvertToConstant := True;
  CaseBooleanFunction.OptionalArguments := -1;
  CaseBooleanFunction.BFunctionAddr := @_CaseBoolean;
  Add(CaseBooleanFunction);

  CaseIntegerFunction.ResultType := rdtInteger;
  CaseIntegerFunction.Name := 'CaseI';
  CaseIntegerFunction.Prototype :=
    StrLogical+'CaseI(Index, Integer_Result1, Integer_Result2, ...)';
  SetLength(CaseIntegerFunction.InputDataTypes, 4);
  CaseIntegerFunction.InputDataTypes[0] := rdtInteger;
  CaseIntegerFunction.InputDataTypes[1] := rdtInteger;
  CaseIntegerFunction.InputDataTypes[2] := rdtInteger;
  CaseIntegerFunction.InputDataTypes[3] := rdtInteger;
  CaseIntegerFunction.OptionalArguments := -1;
  CaseIntegerFunction.CanConvertToConstant := True;
  CaseIntegerFunction.IFunctionAddr := @_CaseInteger;
  Add(CaseIntegerFunction);

  CaseDoubleFunction.ResultType := rdtDouble;
  CaseDoubleFunction.Name := 'CaseR';
  CaseDoubleFunction.Prototype :=
    StrLogical+'CaseR(Index, Real_Result1, Real_Result2, ...)';
  SetLength(CaseDoubleFunction.InputDataTypes, 4);
  CaseDoubleFunction.InputDataTypes[0] := rdtInteger;
  CaseDoubleFunction.InputDataTypes[1] := rdtDouble;
  CaseDoubleFunction.InputDataTypes[2] := rdtDouble;
  CaseDoubleFunction.InputDataTypes[3] := rdtDouble;
  CaseDoubleFunction.OptionalArguments := -1;
  CaseDoubleFunction.CanConvertToConstant := True;
  CaseDoubleFunction.RFunctionAddr := @_CaseDouble;
  Add(CaseDoubleFunction);

  CaseStringFunction.ResultType := rdtString;
  CaseStringFunction.Name := 'CaseT';
  CaseStringFunction.Prototype :=
    StrLogical+'CaseT(Index, Text_Result1, Text_Result2, ...)';
  SetLength(CaseStringFunction.InputDataTypes, 4);
  CaseStringFunction.InputDataTypes[0] := rdtInteger;
  CaseStringFunction.InputDataTypes[1] := rdtString;
  CaseStringFunction.InputDataTypes[2] := rdtString;
  CaseStringFunction.InputDataTypes[3] := rdtString;
  CaseStringFunction.OptionalArguments := -1;
  CaseStringFunction.CanConvertToConstant := True;
  CaseStringFunction.SFunctionAddr := @_CaseString;
  SetLength(CaseStringFunction.Synonyms, 1);
  CaseStringFunction.Synonyms[0] := 'CaseS';
  Add(CaseStringFunction);

  ClosestFunction.ResultType := rdtInteger;
  ClosestFunction.Name := 'Closest';
  ClosestFunction.Prototype :=
    StrMath+'Closest(TestValue, Value1, Value2, ...)';
  SetLength(ClosestFunction.InputDataTypes, 2);
  ClosestFunction.InputDataTypes[0] := rdtDouble;
  ClosestFunction.InputDataTypes[1] := rdtDouble;
  ClosestFunction.OptionalArguments := -1;
  ClosestFunction.CanConvertToConstant := True;
  ClosestFunction.IFunctionAddr := @_Closest;
  Add(ClosestFunction);


  DegToRadFunction.ResultType := rdtDouble;
  DegToRadFunction.Name := 'DegToRad';
  DegToRadFunction.Prototype := StrTrig+'DegToRad(Value)';
  SetLength(DegToRadFunction.InputDataTypes, 1);
  DegToRadFunction.InputDataTypes[0] := rdtDouble;
  DegToRadFunction.OptionalArguments := 0;
  DegToRadFunction.CanConvertToConstant := True;
  DegToRadFunction.RFunctionAddr := @_DegToRad;
  Add(DegToRadFunction);

  ExpFunction.ResultType := rdtDouble;
  ExpFunction.Name := 'Exp';
  ExpFunction.Prototype := StrMath+'Exp(Value)';
  SetLength(ExpFunction.InputDataTypes, 1);
  ExpFunction.InputDataTypes[0] := rdtDouble;
  ExpFunction.OptionalArguments := 0;
  ExpFunction.CanConvertToConstant := True;
  ExpFunction.RFunctionAddr := @_Exp;
  Add(ExpFunction);

  FactorialFunction.ResultType := rdtInteger;
  FactorialFunction.Name := 'FactorialI';
  FactorialFunction.Prototype := StrMath+'FactorialI(Value_Less_Than_13)';
  SetLength(FactorialFunction.InputDataTypes, 1);
  FactorialFunction.InputDataTypes[0] := rdtInteger;
  FactorialFunction.OptionalArguments := 0;
  FactorialFunction.CanConvertToConstant := True;
  FactorialFunction.IFunctionAddr := @_Factorial;
  Add(FactorialFunction);

  FactorialFFunction.ResultType := rdtDouble;
  FactorialFFunction.Name := 'FactorialR';
  FactorialFFunction.Prototype := StrMath+'FactorialR(Value_Less_Than_171)';
  SetLength(FactorialFFunction.InputDataTypes, 1);
  FactorialFFunction.InputDataTypes[0] := rdtInteger;
  FactorialFFunction.OptionalArguments := 0;
  FactorialFFunction.CanConvertToConstant := True;
  FactorialFFunction.RFunctionAddr := @_FactorialR;
  Add(FactorialFFunction);

  FracFunction.ResultType := rdtDouble;
  FracFunction.Name := 'Frac';
  FracFunction.Prototype := StrMath+'Frac(Value)';
  SetLength(FracFunction.InputDataTypes, 1);
  FracFunction.InputDataTypes[0] := rdtDouble;
  FracFunction.OptionalArguments := 0;
  FracFunction.CanConvertToConstant := True;
  FracFunction.RFunctionAddr := @_Frac;
  Add(FracFunction);

  FloatToStrFunction.ResultType := rdtString;
  FloatToStrFunction.Name := 'FloatToText';
  FloatToStrFunction.Prototype := StrText+'FloatToText(Value)';
  SetLength(FloatToStrFunction.InputDataTypes, 1);
  FloatToStrFunction.InputDataTypes[0] := rdtDouble;
  FloatToStrFunction.OptionalArguments := 0;
  FloatToStrFunction.CanConvertToConstant := True;
  FloatToStrFunction.SFunctionAddr := @_FloatToStr;
  SetLength(FloatToStrFunction.Synonyms, 1);
  FloatToStrFunction.Synonyms[0] := 'FloatToStr';
  Add(FloatToStrFunction);

  IfBooleanFunction.ResultType := rdtBoolean;
  IfBooleanFunction.Name := 'IfB';
  IfBooleanFunction.Prototype :=
    StrLogical+'IfB(Boolean_Value, If_True_Boolean_Result, If_False_Boolean_Result)';
  SetLength(IfBooleanFunction.InputDataTypes, 3);
  IfBooleanFunction.InputDataTypes[0] := rdtBoolean;
  IfBooleanFunction.InputDataTypes[1] := rdtBoolean;
  IfBooleanFunction.InputDataTypes[2] := rdtBoolean;
  IfBooleanFunction.OptionalArguments := 0;
  IfBooleanFunction.CanConvertToConstant := True;
  IfBooleanFunction.BFunctionAddr := @_IfBoolean;
  Add(IfBooleanFunction);

  IfIntegerFunction.ResultType := rdtInteger;
  IfIntegerFunction.Name := 'IfI';
  IfIntegerFunction.Prototype :=
    StrLogical+'IfI(Boolean_Value, If_True_Integer_Result, If_False_Integer_Result)';
  SetLength(IfIntegerFunction.InputDataTypes, 3);
  IfIntegerFunction.InputDataTypes[0] := rdtBoolean;
  IfIntegerFunction.InputDataTypes[1] := rdtInteger;
  IfIntegerFunction.InputDataTypes[2] := rdtInteger;
  IfIntegerFunction.OptionalArguments := 0;
  IfIntegerFunction.CanConvertToConstant := True;
  IfIntegerFunction.IFunctionAddr := @_IfInteger;
  Add(IfIntegerFunction);

  IfRealFunction.ResultType := rdtDouble;
  IfRealFunction.Name := 'IfR';
  IfRealFunction.Prototype :=
    StrLogical+'IfR(Boolean_Value, If_True_Real_Result, If_False_Real_Result)';
  SetLength(IfRealFunction.InputDataTypes, 3);
  IfRealFunction.InputDataTypes[0] := rdtBoolean;
  IfRealFunction.InputDataTypes[1] := rdtDouble;
  IfRealFunction.InputDataTypes[2] := rdtDouble;
  IfRealFunction.OptionalArguments := 0;
  IfRealFunction.CanConvertToConstant := True;
  IfRealFunction.RFunctionAddr := @_IfDouble;
  Add(IfRealFunction);

  IfStringFunction.ResultType := rdtString;
  IfStringFunction.Name := 'IfT';
  IfStringFunction.Prototype :=
    StrLogical+'IfT(Boolean_Value, If_True_Text_Result, If_False_Text_Result)';
  SetLength(IfStringFunction.InputDataTypes, 3);
  IfStringFunction.InputDataTypes[0] := rdtBoolean;
  IfStringFunction.InputDataTypes[1] := rdtString;
  IfStringFunction.InputDataTypes[2] := rdtString;
  IfStringFunction.OptionalArguments := 0;
  IfStringFunction.CanConvertToConstant := True;
  IfStringFunction.SFunctionAddr := @_IfString;
  SetLength(IfStringFunction.Synonyms, 1);
  IfStringFunction.Synonyms[0] := 'IfS';
  Add(IfStringFunction);

  IntPowerFunction.ResultType := rdtDouble;
  IntPowerFunction.Name := 'IntPower';
  IntPowerFunction.Prototype := StrMath+'IntPower(Base, Exponent)';
  SetLength(IntPowerFunction.InputDataTypes, 2);
  IntPowerFunction.InputDataTypes[0] := rdtDouble;
  IntPowerFunction.InputDataTypes[1] := rdtInteger;
  IntPowerFunction.OptionalArguments := 0;
  IntPowerFunction.CanConvertToConstant := True;
  IntPowerFunction.RFunctionAddr := @_IntPower;
  Add(IntPowerFunction);

  IntToStrFunction.ResultType := rdtString;
  IntToStrFunction.Name := 'IntToText';
  IntToStrFunction.Prototype := StrText+'IntToText(Value)';
  SetLength(IntToStrFunction.InputDataTypes, 1);
  IntToStrFunction.InputDataTypes[0] := rdtInteger;
  IntToStrFunction.OptionalArguments := 0;
  IntToStrFunction.CanConvertToConstant := True;
  IntToStrFunction.SFunctionAddr := @_IntToStr;
  SetLength(IntToStrFunction.Synonyms, 1);
  IntToStrFunction.Synonyms[0] := 'IntToStr';
  Add(IntToStrFunction);

  LengthFunction.ResultType := rdtInteger;
  LengthFunction.Name := 'Length';
  LengthFunction.Prototype := StrText+'Length(Text_Value)';
  SetLength(LengthFunction.InputDataTypes, 1);
  LengthFunction.InputDataTypes[0] := rdtString;
  LengthFunction.OptionalArguments := 0;
  LengthFunction.CanConvertToConstant := True;
  LengthFunction.IFunctionAddr := @_length;
  Add(LengthFunction);

  LnFunction.ResultType := rdtDouble;
  LnFunction.Name := 'ln';
  LnFunction.Prototype := StrMath+'ln(Value)';
  SetLength(LnFunction.InputDataTypes, 1);
  LnFunction.InputDataTypes[0] := rdtDouble;
  LnFunction.OptionalArguments := 0;
  LnFunction.CanConvertToConstant := True;
  LnFunction.RFunctionAddr := @_ln;
  Add(LnFunction);

  Log10Function.ResultType := rdtDouble;
  Log10Function.Name := 'log10';
  Log10Function.Prototype := StrMath+'log10(Value)';
  SetLength(Log10Function.InputDataTypes, 1);
  Log10Function.InputDataTypes[0] := rdtDouble;
  Log10Function.OptionalArguments := 0;
  Log10Function.CanConvertToConstant := True;
  Log10Function.RFunctionAddr := @_log10;
  Add(Log10Function);

  LogNFunction.ResultType := rdtDouble;
  LogNFunction.Name := 'logN';
  LogNFunction.Prototype := StrMath+'logN(Base, Value)';
  SetLength(LogNFunction.InputDataTypes, 2);
  LogNFunction.InputDataTypes[0] := rdtDouble;
  LogNFunction.InputDataTypes[1] := rdtDouble;
  LogNFunction.OptionalArguments := 0;
  LogNFunction.CanConvertToConstant := True;
  LogNFunction.RFunctionAddr := @_logN;
  Add(LogNFunction);

  LowerCaseFunction.ResultType := rdtString;
  LowerCaseFunction.Name := 'LowerCase';
  LowerCaseFunction.Prototype := StrText+'LowerCase(Text_Value)';
  SetLength(LowerCaseFunction.InputDataTypes, 1);
  LowerCaseFunction.InputDataTypes[0] := rdtString;
  LowerCaseFunction.OptionalArguments := 0;
  LowerCaseFunction.CanConvertToConstant := True;
  LowerCaseFunction.SFunctionAddr := @_LowerCase;
  Add(LowerCaseFunction);

  MaxIFunction.ResultType := rdtInteger;
  MaxIFunction.Name := 'MaxI';
  MaxIFunction.Prototype := StrMath+'MaxI(Integer_Value1, Integer_Value2, ...)';
  SetLength(MaxIFunction.InputDataTypes, 3);
  MaxIFunction.InputDataTypes[0] := rdtInteger;
  MaxIFunction.InputDataTypes[1] := rdtInteger;
  MaxIFunction.InputDataTypes[2] := rdtInteger;
  MaxIFunction.OptionalArguments := -1;
  MaxIFunction.CanConvertToConstant := True;
  MaxIFunction.IFunctionAddr := @_maxI;
  Add(MaxIFunction);

  MaxRFunction.ResultType := rdtDouble;
  MaxRFunction.Name := 'MaxR';
  MaxRFunction.Prototype := StrMath+'MaxR(Real_Value1, Real_Value2, ...)';
  SetLength(MaxRFunction.InputDataTypes, 3);
  MaxRFunction.InputDataTypes[0] := rdtDouble;
  MaxRFunction.InputDataTypes[1] := rdtDouble;
  MaxRFunction.InputDataTypes[2] := rdtDouble;
  MaxRFunction.OptionalArguments := -1;
  MaxRFunction.CanConvertToConstant := True;
  MaxRFunction.RFunctionAddr := @_maxR;
  Add(MaxRFunction);

  MinIFunction.ResultType := rdtInteger;
  MinIFunction.Name := 'MinI';
  MinIFunction.Prototype := StrMath+'MinI(Integer_Value1, Integer_Value2, ...)';
  SetLength(MinIFunction.InputDataTypes, 3);
  MinIFunction.InputDataTypes[0] := rdtInteger;
  MinIFunction.InputDataTypes[1] := rdtInteger;
  MinIFunction.InputDataTypes[2] := rdtInteger;
  MinIFunction.OptionalArguments := -1;
  MinIFunction.CanConvertToConstant := True;
  MinIFunction.IFunctionAddr := @_minI;
  Add(MinIFunction);

  MinRFunction.ResultType := rdtDouble;
  MinRFunction.Name := 'MinR';
  MinRFunction.Prototype := StrMath+'MinR(Real_Value1, Real_Value2, ...)';
  SetLength(MinRFunction.InputDataTypes, 3);
  MinRFunction.InputDataTypes[0] := rdtDouble;
  MinRFunction.InputDataTypes[1] := rdtDouble;
  MinRFunction.InputDataTypes[2] := rdtDouble;
  MinRFunction.OptionalArguments := -1;
  MinRFunction.CanConvertToConstant := True;
  MinRFunction.RFunctionAddr := @_minR;
  Add(MinRFunction);

  OddFunction.ResultType := rdtBoolean;
  OddFunction.Name := 'Odd';
  OddFunction.Prototype := StrMath+'Odd(Value)';
  SetLength(OddFunction.InputDataTypes, 1);
  OddFunction.InputDataTypes[0] := rdtInteger;
  OddFunction.CanConvertToConstant := True;
  OddFunction.BFunctionAddr := @_Odd;
  Add(OddFunction);

  PiFunction.ResultType := rdtDouble;
  PiFunction.Name := 'Pi';
  PiFunction.Prototype := StrMath+'Pi';
  SetLength(PiFunction.InputDataTypes, 0);
  PiFunction.OptionalArguments := 0;
  PiFunction.CanConvertToConstant := True;
  PiFunction.RFunctionAddr := @_Pi;
  Add(PiFunction);

  PosFunction.ResultType := rdtInteger;
  PosFunction.Name := 'Pos';
  PosFunction.Prototype := StrText+'Pos(SubText, Text_Value)';
  SetLength(PosFunction.InputDataTypes, 2);
  PosFunction.InputDataTypes[0] := rdtString;
  PosFunction.InputDataTypes[1] := rdtString;
  PosFunction.OptionalArguments := 0;
  PosFunction.CanConvertToConstant := True;
  PosFunction.IFunctionAddr := @_Pos;
  Add(PosFunction);

  PosExFunction.ResultType := rdtInteger;
  PosExFunction.Name := 'PosEx';
  PosExFunction.Prototype := StrText+'PosEx(SubText, Text_Value, Offset)';
  SetLength(PosExFunction.InputDataTypes, 3);
  PosExFunction.InputDataTypes[0] := rdtString;
  PosExFunction.InputDataTypes[1] := rdtString;
  PosExFunction.InputDataTypes[2] := rdtInteger;
  PosExFunction.OptionalArguments := 0;
  PosExFunction.CanConvertToConstant := True;
  PosExFunction.IFunctionAddr := @_PosEx;
  Add(PosExFunction);


  PositionInList.ResultType := rdtInteger;
  PositionInList.Name := 'PositionInList';
  PositionInList.Prototype :=
    StrText+'PositionInList(TestTextItem, FirstTextItem, SecondTextItem, ...)';
  SetLength(PositionInList.InputDataTypes, 2);
  PositionInList.InputDataTypes[0] := rdtString;
  PositionInList.InputDataTypes[1] := rdtString;
  PositionInList.CanConvertToConstant := True;
  PositionInList.OptionalArguments := -1;
  PositionInList.IFunctionAddr := @_PositionInList;
  Add(PositionInList);



  PowerFunction.ResultType := rdtDouble;
  PowerFunction.Name := 'Power';
  PowerFunction.Prototype := StrMath+'Power(Base, Exponent)';
  SetLength(PowerFunction.InputDataTypes, 2);
  PowerFunction.InputDataTypes[0] := rdtDouble;
  PowerFunction.InputDataTypes[1] := rdtDouble;
  PowerFunction.OptionalArguments := 0;
  PowerFunction.CanConvertToConstant := True;
  PowerFunction.RFunctionAddr := @_Power;
  Add(PowerFunction);

  RadToDegFunction.ResultType := rdtDouble;
  RadToDegFunction.Name := 'RadToDeg';
  RadToDegFunction.Prototype := StrTrig+'RadToDeg(Value)';
  SetLength(RadToDegFunction.InputDataTypes, 1);
  RadToDegFunction.InputDataTypes[0] := rdtDouble;
  RadToDegFunction.OptionalArguments := 0;
  RadToDegFunction.CanConvertToConstant := True;
  RadToDegFunction.RFunctionAddr := @_RadToDeg;
  Add(RadToDegFunction);

  RoundFunction.ResultType := rdtInteger;
  RoundFunction.Name := 'Round';
  RoundFunction.Prototype := StrMath+'Round(Value)';
  SetLength(RoundFunction.InputDataTypes, 1);
  RoundFunction.InputDataTypes[0] := rdtDouble;
  RoundFunction.OptionalArguments := 0;
  RoundFunction.CanConvertToConstant := True;
  RoundFunction.IFunctionAddr := @_Round;
  Add(RoundFunction);

  SinFunction.ResultType := rdtDouble;
  SinFunction.Name := 'Sin';
  SinFunction.Prototype := StrTrig+'Sin(Value)';
  SetLength(SinFunction.InputDataTypes, 1);
  SinFunction.InputDataTypes[0] := rdtDouble;
  SinFunction.OptionalArguments := 0;
  SinFunction.CanConvertToConstant := True;
  SinFunction.RFunctionAddr := @_sin;
  Add(SinFunction);

  SinhFunction.ResultType := rdtDouble;
  SinhFunction.Name := 'Sinh';
  SinhFunction.Prototype := StrTrig+'Sinh(Value)';
  SetLength(SinhFunction.InputDataTypes, 1);
  SinhFunction.InputDataTypes[0] := rdtDouble;
  SinhFunction.OptionalArguments := 0;
  SinhFunction.CanConvertToConstant := True;
  SinhFunction.RFunctionAddr := @_sinh;
  Add(SinhFunction);

  SqrIFunction.ResultType := rdtInteger;
  SqrIFunction.Name := 'SqrI';
  SqrIFunction.Prototype := StrMath+'SqrI(Integer_Value)';
  SetLength(SqrIFunction.InputDataTypes, 1);
  SqrIFunction.InputDataTypes[0] := rdtInteger;
  SqrIFunction.OptionalArguments := 0;
  SqrIFunction.CanConvertToConstant := True;
  SqrIFunction.IFunctionAddr := @_SqrI;
  Add(SqrIFunction);

  SqrRFunction.ResultType := rdtDouble;
  SqrRFunction.Name := 'SqrR';
  SqrRFunction.Prototype := StrMath+'SqrR(Real_Value)';
  SetLength(SqrRFunction.InputDataTypes, 1);
  SqrRFunction.InputDataTypes[0] := rdtDouble;
  SqrRFunction.OptionalArguments := 0;
  SqrRFunction.CanConvertToConstant := True;
  SqrRFunction.RFunctionAddr := @_SqrR;
  Add(SqrRFunction);

  SqrtFunction.ResultType := rdtDouble;
  SqrtFunction.Name := 'Sqrt';
  SqrtFunction.Prototype := StrMath+'Sqrt(Value)';
  SetLength(SqrtFunction.InputDataTypes, 1);
  SqrtFunction.InputDataTypes[0] := rdtDouble;
  SqrtFunction.OptionalArguments := 0;
  SqrtFunction.CanConvertToConstant := True;
  SqrtFunction.RFunctionAddr := @_sqrt;
  Add(SqrtFunction);

  StrToIntFunction.ResultType := rdtInteger;
  StrToIntFunction.Name := 'TextToInt';
  StrToIntFunction.Prototype := StrText+'TextToInt(Value)';
  SetLength(StrToIntFunction.InputDataTypes, 1);
  StrToIntFunction.InputDataTypes[0] := rdtString;
  StrToIntFunction.OptionalArguments := 0;
  StrToIntFunction.CanConvertToConstant := True;
  StrToIntFunction.IFunctionAddr := @_StrToInt;
  SetLength(StrToIntFunction.Synonyms, 1);
  StrToIntFunction.Synonyms[0] := 'StrToInt';
  Add(StrToIntFunction);

  StrToIntDefFunction.ResultType := rdtInteger;
  StrToIntDefFunction.Name := 'TextToIntDef';
  StrToIntDefFunction.Prototype := StrText+'TextToIntDef(Value, DefaultResult)';
  SetLength(StrToIntDefFunction.InputDataTypes, 2);
  StrToIntDefFunction.InputDataTypes[0] := rdtString;
  StrToIntDefFunction.InputDataTypes[1] := rdtInteger;
  StrToIntDefFunction.OptionalArguments := 0;
  StrToIntDefFunction.CanConvertToConstant := True;
  StrToIntDefFunction.IFunctionAddr := @_StrToIntDef;
  SetLength(StrToIntDefFunction.Synonyms, 1);
  StrToIntDefFunction.Synonyms[0] := 'StrToIntDef';
  Add(StrToIntDefFunction);

  StrToFloatFunction.ResultType := rdtDouble;
  StrToFloatFunction.Name := 'TextToFloat';
  StrToFloatFunction.Prototype := StrText+'TextToFloat(Text_Value)';
  SetLength(StrToFloatFunction.InputDataTypes, 1);
  StrToFloatFunction.InputDataTypes[0] := rdtString;
  StrToFloatFunction.OptionalArguments := 0;
  StrToFloatFunction.CanConvertToConstant := True;
  StrToFloatFunction.RFunctionAddr := @_StrToFloat;
  SetLength(StrToFloatFunction.Synonyms, 1);
  StrToFloatFunction.Synonyms[0] := 'StrToFloat';
  Add(StrToFloatFunction);

  StrToFloatDefFunction.ResultType := rdtDouble;
  StrToFloatDefFunction.Name := 'TextToFloatDef';
  StrToFloatDefFunction.Prototype :=
    StrText+'TextToFloatDef(Text_Value, DefaultResult)';
  SetLength(StrToFloatDefFunction.InputDataTypes, 2);
  StrToFloatDefFunction.InputDataTypes[0] := rdtString;
  StrToFloatDefFunction.InputDataTypes[1] := rdtDouble;
  StrToFloatDefFunction.OptionalArguments := 0;
  StrToFloatDefFunction.CanConvertToConstant := True;
  StrToFloatDefFunction.RFunctionAddr := @_StrToFloatDef;
  SetLength(StrToFloatDefFunction.Synonyms, 1);
  StrToFloatDefFunction.Synonyms[0] := 'StrToFloatDef';
  Add(StrToFloatDefFunction);

  TanFunction.ResultType := rdtDouble;
  TanFunction.Name := 'Tan';
  TanFunction.Prototype := StrTrig+'Tan(Value)';
  SetLength(TanFunction.InputDataTypes, 1);
  TanFunction.InputDataTypes[0] := rdtDouble;
  TanFunction.OptionalArguments := 0;
  TanFunction.CanConvertToConstant := True;
  TanFunction.RFunctionAddr := @_tan;
  Add(TanFunction);

  TanhFunction.ResultType := rdtDouble;
  TanhFunction.Name := 'Tanh';
  TanhFunction.Prototype := StrTrig+'Tanh(Value)';
  SetLength(TanhFunction.InputDataTypes, 1);
  TanhFunction.InputDataTypes[0] := rdtDouble;
  TanhFunction.OptionalArguments := 0;
  TanhFunction.CanConvertToConstant := True;
  TanhFunction.RFunctionAddr := @_tanh;
  Add(TanhFunction);

  TrimMunction.ResultType := rdtString;
  TrimMunction.Name := 'Trim';
  TrimMunction.Prototype := StrText+'Trim(Text_Value)';
  SetLength(TrimMunction.InputDataTypes, 1);
  TrimMunction.InputDataTypes[0] := rdtString;
  TrimMunction.OptionalArguments := 0;
  TrimMunction.CanConvertToConstant := True;
  TrimMunction.SFunctionAddr := @_Trim;
  Add(TrimMunction);

  TruncFunction.ResultType := rdtInteger;
  TruncFunction.Name := 'Trunc';
  TruncFunction.Prototype := StrMath+'Trunc(Value)';
  SetLength(TruncFunction.InputDataTypes, 1);
  TruncFunction.InputDataTypes[0] := rdtDouble;
  TruncFunction.OptionalArguments := 0;
  TruncFunction.CanConvertToConstant := True;
  TruncFunction.IFunctionAddr := @_trunc;
  Add(TruncFunction);

  UpperCaseFunction.ResultType := rdtString;
  UpperCaseFunction.Name := 'UpperCase';
  UpperCaseFunction.Prototype := StrText+'UpperCase(Text_Value)';
  SetLength(UpperCaseFunction.InputDataTypes, 1);
  UpperCaseFunction.InputDataTypes[0] := rdtString;
  UpperCaseFunction.OptionalArguments := 0;
  UpperCaseFunction.CanConvertToConstant := True;
  UpperCaseFunction.SFunctionAddr := @_UpperCase;
  Add(UpperCaseFunction);

  InterpolateFunction.ResultType := rdtDouble;
  InterpolateFunction.Name := 'Interpolate';
  InterpolateFunction.Prototype :=
    StrMath+'Interpolate(Position, Value1, Distance1, Value2, Distance2)';
  SetLength(InterpolateFunction.InputDataTypes, 5);
  InterpolateFunction.InputDataTypes[0] := rdtDouble;
  InterpolateFunction.InputDataTypes[1] := rdtDouble;
  InterpolateFunction.InputDataTypes[2] := rdtDouble;
  InterpolateFunction.InputDataTypes[3] := rdtDouble;
  InterpolateFunction.InputDataTypes[4] := rdtDouble;
  InterpolateFunction.OptionalArguments := 0;
  InterpolateFunction.CanConvertToConstant := True;
  InterpolateFunction.RFunctionAddr := @_Interpolate;
  Add(InterpolateFunction);

  DistanceFunction.ResultType := rdtDouble;
  DistanceFunction.Name := 'Distance';
  DistanceFunction.Prototype := StrMath+'Distance(X1, Y1, X2, Y2)';
  SetLength(DistanceFunction.InputDataTypes, 4);
  DistanceFunction.InputDataTypes[0] := rdtDouble;
  DistanceFunction.InputDataTypes[1] := rdtDouble;
  DistanceFunction.InputDataTypes[2] := rdtDouble;
  DistanceFunction.InputDataTypes[3] := rdtDouble;
  DistanceFunction.OptionalArguments := 0;
  DistanceFunction.CanConvertToConstant := True;
  DistanceFunction.RFunctionAddr := @_Distance;
  Add(DistanceFunction);
end;

procedure TFunctionStringList.Delete(Index: Integer);
var
  AFunctionClass: TFunctionClass;
  Position: integer;
begin
  AFunctionClass := Objects[Index] as TFunctionClass;
  inherited;
  Position := IndexOfObject(AFunctionClass);
  while Position >= 0 do
  begin
    inherited Delete(Position);
    Position := IndexOfObject(AFunctionClass);
  end;
  AFunctionClass.Free;
end;

destructor TFunctionStringList.Destroy;
begin
  Clear;
  inherited;
end;

function TFunctionStringList.GetFunctionClass(
  const Index: integer): TFunctionClass;
begin
  Result := Objects[Index] as TFunctionClass;
end;

{ TAndExpression }

procedure TAndExpression.Evaluate;
var
  Index: integer;
  ArrayLength: integer;
  AVariable: TConstant;
begin
  if ShouldEvaluate then
  begin
    ArrayLength := Length(Data);
    Assert(ArrayLength = 2);
    Assert(ResultType = rdtBoolean);
    for Index := 0 to ArrayLength - 1 do
    begin
      if Data[Index].Datum = nil then
      begin
        Assert(False);
      end
      else
      begin
        AVariable := TConstant(Data[Index].Datum);
        if AVariable is TExpression then
        begin
          TExpression(AVariable).Evaluate;
        end;
        SetResultFromVariable(AVariable);
        if not GetResult then
          Exit;
      end;
    end;
  end;
end;

{$WARNINGS OFF}
function TPartialEvaluationOperator.GetResult: Boolean;
begin
  result := PBoolean(FResult)^
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TPartialEvaluationOperator.SetResultFromVariable(AVariable: TConstant);
begin
  PBoolean(FResult)^ := PBoolean(AVariable.FResult)^;
end;
{$WARNINGS ON}

{ TOrExpression }

procedure TOrExpression.Evaluate;
var
  Index: integer;
  ArrayLength: integer;
  AVariable: TConstant;
begin
  if ShouldEvaluate then
  begin
    ArrayLength := Length(Data);
    Assert(ArrayLength = 2);
    Assert(ResultType = rdtBoolean);
    for Index := 0 to ArrayLength - 1 do
    begin
      if Data[Index].Datum = nil then
      begin
        Assert(False);
      end
      else
      begin
        AVariable := TConstant(Data[Index].Datum);
        if AVariable is TExpression then
        begin
          TExpression(AVariable).Evaluate;
        end;
        SetResultFromVariable(AVariable);
//        PBoolean(FResult)^ := PBoolean(AVariable.FResult)^;
        if GetResult then
          Exit;
      end;
    end;
  end;
end;

{ TSelectExpression }

{$WARNINGS OFF}
function TSelectExpression.GetSelectIndex(AVariable: TConstant): integer;
begin
  if Data[0].DataType = rdtInteger then
  begin
    result := PInteger(AVariable.FResult)^;
  end
  else
  begin
    result := 2 - Ord(pBoolean(AVariable.FResult)^);
  end;
end;
{$WARNINGS ON}

procedure TSelectExpression.Evaluate;
var
  ArrayLength: integer;
  AVariable: TConstant;
  SelectIndex: integer;
begin
  if ShouldEvaluate then
  begin
    ArrayLength := Length(Data);
    Assert(ArrayLength >= 3);
    SelectIndex := 0;
    case Data[0].DataType of
      rdtInteger, rdtBoolean:
        begin
          AVariable := TConstant(Data[0].Datum);
          if AVariable is TExpression then
          begin
            TExpression(AVariable).Evaluate;
          end;
          SelectIndex := GetSelectIndex(AVariable);
//          if Data[0].DataType = rdtInteger then
//          begin
//            SelectIndex := PInteger(AVariable.FResult)^;
//          end
//          else
//          begin
//            SelectIndex := 2 - Ord(pBoolean(AVariable.FResult)^);
//          end;
        end;
    else
      Assert(False);
    end;
    if (SelectIndex <= 0) then
    begin
      raise ERbwParserError.Create(Format(StrErrorUnableToEva,
        [Name, SelectIndex]));
    end;

    if (SelectIndex >= ArrayLength) then
    begin
      raise ERbwParserError.Create(Format(StrErrorUnableToEva2,
        [Name, SelectIndex, ArrayLength, SelectIndex + 1]));
    end;

    if Data[SelectIndex].Datum = nil then
    begin
      Assert(False);
    end
    else
    begin
      AVariable := TConstant(Data[SelectIndex].Datum);
      Assert(AVariable.FResultType = FResultType);
      if AVariable is TExpression then
      begin
        TExpression(AVariable).Evaluate;
      end;
      SetResultFromConstant(AVariable, self);
//      case ResultType of
//        rdtDouble:
//          begin
//            PDouble(FResult)^ := PDouble(AVariable.FResult)^;
//          end;
//        rdtInteger:
//          begin
//            PInteger(FResult)^ := PInteger(AVariable.FResult)^;
//          end;
//        rdtBoolean:
//          begin
//            PBoolean(FResult)^ := PBoolean(AVariable.FResult)^;
//          end;
//        rdtString:
//          begin
//            ResultString := AVariable.ResultString;
//          end;
//      else
//        Assert(False);
//      end;
    end;
  end;
end;


{ TCustomVariable }

constructor TCustomVariable.Create(const VariableName: string;
  const DataType: TRbwDataType; const NameToDisplay: string);
begin
  inherited Create(VariableName, DataType);
  ValidateVariableName(VariableName);
  DisplayName := NameToDisplay;
  ValidateVariableName(DisplayName);
end;

function TCustomVariable.Decompile: string;
begin
  result := Name;
end;

function TCustomVariable.DecompileDisplay: string;
begin
  result := DisplayName;
end;

function TCustomVariable.GetDisplayName: string;
begin
  if FDisplayName = '' then
  begin
    result := inherited Decompile;
  end
  else
  begin
    result := FDisplayName
  end;
end;

procedure TCustomVariable.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

{ TOperator }

function TOperator.Decompile: string;
begin
  result := DecompileByType(dtInternal);
end;

function TOperator.DecompileByType(DecompileType: TDecompileType): string;
var
  ArrayLength: integer;
  AVariable: TConstant;
  Paren: string;
begin
  if Assigned(FunctionAddr) then
  begin
    result := Name;
    ArrayLength := Length(Data);
    Assert(ArrayLength in [1, 2]);
    if ArrayLength = 1 then
    begin
      AVariable := TConstant(Data[0].Datum);
      Assert(AVariable <> nil);
      case DecompileType of
        dtInternal: result := result + ' ' + AVariable.Decompile;
        dtDisplay: result := result + ' ' + AVariable.DecompileDisplay;
        else Assert(False);
      end;
    end
    else
    begin
      AVariable := TConstant(Data[0].Datum);
      Assert(AVariable <> nil);
      if FTopLevel then
      begin
        Paren := '';
      end
      else
      begin
        Paren := '(';
      end;
      if AVariable is TExpression then
      begin
        TExpression(AVariable).FTopLevel := False;
      end;

      case DecompileType of
        dtInternal: result := Paren + AVariable.Decompile + ' ' + result + ' ';
        dtDisplay: result := Paren + AVariable.DecompileDisplay + ' ' + result + ' ';
        else Assert(False);
      end;

      AVariable := TConstant(Data[1].Datum);
      Assert(AVariable <> nil);
      if AVariable is TExpression then
      begin
        TExpression(AVariable).FTopLevel := False;
      end;
      case DecompileType of
        dtInternal: result := result + AVariable.Decompile;
        dtDisplay: result := result + AVariable.DecompileDisplay;
        else Assert(False);
      end;

      if not FTopLevel then
      begin
        result := result + ')';
      end;
    end;
  end
  else
  begin
    result := ValueToString;
  end;
end;

function TOperator.DecompileDisplay: string;
begin
  result := DecompileByType(dtDisplay);
end;

{ TSignOperator }

function TSignOperator.Decompile: string;
begin
  result := DecompileByType(dtInternal);
end;

function TSignOperator.DecompileByType(DecompileType: TDecompileType): string;
var
  ArrayLength: integer;
  AVariable: TConstant;
begin
  if Assigned(FunctionAddr) then
  begin
    result := Name;
    ArrayLength := Length(Data);
    Assert(ArrayLength = 1);
    AVariable := TConstant(Data[0].Datum);
    Assert(AVariable <> nil);
    if AVariable is TExpression then
    begin
      TExpression(AVariable).FTopLevel := False;
    end;
    case DecompileType of
      dtInternal: result := result + AVariable.Decompile;
      dtDisplay: result := result + AVariable.DecompileDisplay;
      else Assert(False);
    end;

  end
  else
  begin
    result := ValueToString;
  end;
end;

function TSignOperator.DecompileDisplay: string;
begin
  result := DecompileByType(dtDisplay);
end;

{ TIntToDoubleExpression }

function TIntToDoubleExpression.Decompile: string;
begin
  result := DecompileByType(dtInternal);
end;

function TIntToDoubleExpression.DecompileByType(DecompileType: TDecompileType): string;
var
  ArrayLength: integer;
  AVariable: TConstant;
begin
  ArrayLength := Length(Data);
  Assert(ArrayLength = 1);
  AVariable := TConstant(Data[0].Datum);
  Assert(AVariable <> nil);
  if AVariable is TExpression then
  begin
    TExpression(AVariable).FTopLevel := FTopLevel;
  end;
  result := '';
  case DecompileType of
    dtInternal: result := AVariable.Decompile;
    dtDisplay: result := AVariable.DecompileDisplay;
    else Assert(False);
  end;

end;

function TIntToDoubleExpression.DecompileDisplay: string;
begin
  result := DecompileByType(dtDisplay);
end;

procedure InitializeVariables;
//var
//  Index: integer;
//  AFunction: TFunctionClass;
begin
  IntToDoubleFunction := TFunctionClass.Create;
  IntToDoubleFunction.InputDataCount := 1;
  IntToDoubleFunction.RFunctionAddr := @_IntToDouble;
  IntToDoubleFunction.Name := 'IntToDouble';
  IntToDoubleFunction.InputDataTypes[0] := rdtInteger;
  IntToDoubleFunction.OptionalArguments := 0;

  OperatorList := TObjectList.Create;

  NotOperator := TFunctionClass.Create;
  OperatorList.Add(NotOperator);
  NotOperator.InputDataCount := 1;
  NotOperator.BFunctionAddr := @_not;
  NotOperator.Name := 'not';
  NotOperator.Prototype := StrLogical+'not';
  NotOperator.InputDataTypes[0] := rdtBoolean;
  NotOperator.OptionalArguments := 0;

  PowerOperator := TFunctionClass.Create;
  OperatorList.Add(PowerOperator);
  PowerOperator.InputDataCount := 2;
  PowerOperator.RFunctionAddr := @_Power;
  PowerOperator.Name := '^';
  PowerOperator.Prototype := StrMath+'^';
  PowerOperator.InputDataTypes[0] := rdtDouble;
  PowerOperator.InputDataTypes[1] := rdtDouble;
  PowerOperator.OptionalArguments := 0;

  PowerOperator2 := TFunctionClass.Create;
  OperatorList.Add(PowerOperator2);
  PowerOperator2.InputDataCount := 2;
  PowerOperator2.RFunctionAddr := @_Power;
  PowerOperator2.Name := '**';
  PowerOperator2.Prototype := StrMath+'**';
  PowerOperator2.InputDataTypes[0] := rdtDouble;
  PowerOperator2.InputDataTypes[1] := rdtDouble;
  PowerOperator2.OptionalArguments := 0;

  XorOperator := TFunctionClass.Create;
  OperatorList.Add(XorOperator);
  XorOperator.InputDataCount := 2;
  XorOperator.BFunctionAddr := @_Xor;
  XorOperator.Name := 'xor';
  XorOperator.Prototype := StrLogical+'xor';
  XorOperator.InputDataTypes[0] := rdtBoolean;
  XorOperator.InputDataTypes[1] := rdtBoolean;
  XorOperator.OptionalArguments := 0;

  TimesIOperator := TFunctionClass.Create;
  OperatorList.Add(TimesIOperator);
  TimesIOperator.InputDataCount := 2;
  TimesIOperator.IFunctionAddr := @_TimesI;
  TimesIOperator.Name := '*';
  TimesIOperator.Prototype := StrMath+'*';
  TimesIOperator.InputDataTypes[0] := rdtInteger;
  TimesIOperator.InputDataTypes[1] := rdtInteger;
  TimesIOperator.OptionalArguments := 0;

  TimesROperator := TFunctionClass.Create;
  OperatorList.Add(TimesROperator);
  TimesROperator.InputDataCount := 2;
  TimesROperator.RFunctionAddr := @_TimesR;
  TimesROperator.Name := '*';
  TimesROperator.Prototype := StrMath+'*';
  TimesROperator.InputDataTypes[0] := rdtDouble;
  TimesROperator.InputDataTypes[1] := rdtDouble;
  TimesROperator.OptionalArguments := 0;

  DivideROperator := TFunctionClass.Create;
  OperatorList.Add(DivideROperator);
  DivideROperator.InputDataCount := 2;
  DivideROperator.RFunctionAddr := @_DivideR;
  DivideROperator.Name := '/';
  DivideROperator.Prototype := StrMath+'/';
  DivideROperator.InputDataTypes[0] := rdtDouble;
  DivideROperator.InputDataTypes[1] := rdtDouble;
  DivideROperator.OptionalArguments := 0;

  DivideIOperator := TFunctionClass.Create;
  OperatorList.Add(DivideIOperator);
  DivideIOperator.InputDataCount := 2;
  DivideIOperator.RFunctionAddr := @_DivideI;
  DivideIOperator.Name := '/';
  DivideIOperator.Prototype := StrMath+'/';
  DivideIOperator.InputDataTypes[0] := rdtInteger;
  DivideIOperator.InputDataTypes[1] := rdtInteger;
  DivideIOperator.OptionalArguments := 0;

  DivOperator := TFunctionClass.Create;
  OperatorList.Add(DivOperator);
  DivOperator.InputDataCount := 2;
  DivOperator.IFunctionAddr := @_Div;
  DivOperator.Name := 'div';
  DivOperator.Prototype := StrMath+'div';
  DivOperator.InputDataTypes[0] := rdtInteger;
  DivOperator.InputDataTypes[1] := rdtInteger;
  DivOperator.OptionalArguments := 0;

  ModOperator := TFunctionClass.Create;
  OperatorList.Add(ModOperator);
  ModOperator.InputDataCount := 2;
  ModOperator.IFunctionAddr := @_Mod;
  ModOperator.Name := 'mod';
  ModOperator.Prototype := StrMath+'mod';
  ModOperator.InputDataTypes[0] := rdtInteger;
  ModOperator.InputDataTypes[1] := rdtInteger;
  ModOperator.OptionalArguments := 0;

  AndOperator := TFunctionClass.Create;
  OperatorList.Add(AndOperator);
  AndOperator.InputDataCount := 2;
  AndOperator.BFunctionAddr := @_And;
  AndOperator.Name := 'and';
  AndOperator.Prototype := StrLogical+'and';
  AndOperator.InputDataTypes[0] := rdtBoolean;
  AndOperator.InputDataTypes[1] := rdtBoolean;
  AndOperator.OptionalArguments := 0;

  OrOperator := TFunctionClass.Create;
  OperatorList.Add(OrOperator);
  OrOperator.InputDataCount := 2;
  OrOperator.BFunctionAddr := @_Or;
  OrOperator.Name := 'or';
  OrOperator.Prototype := StrLogical+'or';
  OrOperator.InputDataTypes[0] := rdtBoolean;
  OrOperator.InputDataTypes[1] := rdtBoolean;
  OrOperator.OptionalArguments := 0;

  PlusSignIOperator := TFunctionClass.Create;
  OperatorList.Add(PlusSignIOperator);
  PlusSignIOperator.InputDataCount := 1;
  PlusSignIOperator.IFunctionAddr := @_PlusSignI;
  PlusSignIOperator.Name := '+';
  PlusSignIOperator.Prototype := StrMath+'+';
  PlusSignIOperator.InputDataTypes[0] := rdtInteger;
  PlusSignIOperator.OptionalArguments := 0;

  PlusSignROperator := TFunctionClass.Create;
  OperatorList.Add(PlusSignROperator);
  PlusSignROperator.InputDataCount := 1;
  PlusSignROperator.RFunctionAddr := @_PlusSignR;
  PlusSignROperator.Name := '+';
  PlusSignROperator.Prototype := StrMath+'+';
  PlusSignROperator.InputDataTypes[0] := rdtDouble;
  PlusSignROperator.OptionalArguments := 0;

  MinusSignIOperator := TFunctionClass.Create;
  OperatorList.Add(MinusSignIOperator);
  MinusSignIOperator.InputDataCount := 1;
  MinusSignIOperator.IFunctionAddr := @_MinusSignI;
  MinusSignIOperator.Name := '-';
  MinusSignIOperator.Prototype := StrMath+'-';
  MinusSignIOperator.InputDataTypes[0] := rdtInteger;
  MinusSignIOperator.OptionalArguments := 0;

  MinusSignROperator := TFunctionClass.Create;
  OperatorList.Add(MinusSignROperator);
  MinusSignROperator.InputDataCount := 1;
  MinusSignROperator.RFunctionAddr := @_MinusSignR;
  MinusSignROperator.Name := '-';
  MinusSignROperator.Prototype := StrMath+'-';
  MinusSignROperator.InputDataTypes[0] := rdtDouble;
  MinusSignROperator.OptionalArguments := 0;

  PlusIOperator := TFunctionClass.Create;
  OperatorList.Add(PlusIOperator);
  PlusIOperator.InputDataCount := 2;
  PlusIOperator.IFunctionAddr := @_PlusI;
  PlusIOperator.Name := '+';
  PlusIOperator.Prototype := StrMath+'+';
  PlusIOperator.InputDataTypes[0] := rdtInteger;
  PlusIOperator.InputDataTypes[1] := rdtInteger;
  PlusIOperator.OptionalArguments := 0;

  PlusROperator := TFunctionClass.Create;
  OperatorList.Add(PlusROperator);
  PlusROperator.InputDataCount := 2;
  PlusROperator.RFunctionAddr := @_PlusR;
  PlusROperator.Name := '+';
  PlusROperator.Prototype := StrMath+'+';
  PlusROperator.InputDataTypes[0] := rdtDouble;
  PlusROperator.InputDataTypes[1] := rdtDouble;
  PlusROperator.OptionalArguments := 0;

  PlusSOperator := TFunctionClass.Create;
  OperatorList.Add(PlusSOperator);
  PlusSOperator.InputDataCount := 2;
  PlusSOperator.SFunctionAddr := @_PlusS;
  PlusSOperator.Name := '+';
  PlusSOperator.Prototype := StrText+'+';
  PlusSOperator.InputDataTypes[0] := rdtString;
  PlusSOperator.InputDataTypes[1] := rdtString;
  PlusSOperator.OptionalArguments := 0;

  MinusIOperator := TFunctionClass.Create;
  OperatorList.Add(MinusIOperator);
  MinusIOperator.InputDataCount := 2;
  MinusIOperator.IFunctionAddr := @_MinusI;
  MinusIOperator.Name := '-';
  MinusIOperator.Prototype := StrMath+'-';
  MinusIOperator.InputDataTypes[0] := rdtInteger;
  MinusIOperator.InputDataTypes[1] := rdtInteger;
  MinusIOperator.OptionalArguments := 0;

  MinusROperator := TFunctionClass.Create;
  OperatorList.Add(MinusROperator);
  MinusROperator.InputDataCount := 2;
  MinusROperator.RFunctionAddr := @_MinusR;
  MinusROperator.Name := '-';
  MinusROperator.Prototype := StrMath+'-';
  MinusROperator.InputDataTypes[0] := rdtDouble;
  MinusROperator.InputDataTypes[1] := rdtDouble;
  MinusROperator.OptionalArguments := 0;

  EqualIOperator := TFunctionClass.Create;
  OperatorList.Add(EqualIOperator);
  EqualIOperator.InputDataCount := 2;
  EqualIOperator.BFunctionAddr := @_EqualI;
  EqualIOperator.Name := '=';
  EqualIOperator.Prototype := StrLogical+'=';
  EqualIOperator.InputDataTypes[0] := rdtInteger;
  EqualIOperator.InputDataTypes[1] := rdtInteger;
  EqualIOperator.OptionalArguments := 0;

  EqualROperator := TFunctionClass.Create;
  OperatorList.Add(EqualROperator);
  EqualROperator.InputDataCount := 2;
  EqualROperator.BFunctionAddr := @_EqualR;
  EqualROperator.Name := '=';
  EqualROperator.Prototype := StrLogical+'=';
  EqualROperator.InputDataTypes[0] := rdtDouble;
  EqualROperator.InputDataTypes[1] := rdtDouble;
  EqualROperator.OptionalArguments := 0;

  EqualSOperator := TFunctionClass.Create;
  OperatorList.Add(EqualSOperator);
  EqualSOperator.InputDataCount := 2;
  EqualSOperator.BFunctionAddr := @_EqualS;
  EqualSOperator.Name := '=';
  EqualSOperator.Prototype := StrLogical+'=';
  EqualSOperator.InputDataTypes[0] := rdtString;
  EqualSOperator.InputDataTypes[1] := rdtString;
  EqualSOperator.OptionalArguments := 0;

  EqualBOperator := TFunctionClass.Create;
  OperatorList.Add(EqualBOperator);
  EqualBOperator.InputDataCount := 2;
  EqualBOperator.BFunctionAddr := @_EqualB;
  EqualBOperator.Name := '=';
  EqualBOperator.Prototype := StrLogical+'=';
  EqualBOperator.InputDataTypes[0] := rdtBoolean;
  EqualBOperator.InputDataTypes[1] := rdtBoolean;
  EqualBOperator.OptionalArguments := 0;

  NotEqualIOperator := TFunctionClass.Create;
  OperatorList.Add(NotEqualIOperator);
  NotEqualIOperator.InputDataCount := 2;
  NotEqualIOperator.BFunctionAddr := @_NotEqualI;
  NotEqualIOperator.Name := '<>';
  NotEqualIOperator.Prototype := StrLogical+'<>';
  NotEqualIOperator.InputDataTypes[0] := rdtInteger;
  NotEqualIOperator.InputDataTypes[1] := rdtInteger;
  NotEqualIOperator.OptionalArguments := 0;

  NotEqualROperator := TFunctionClass.Create;
  OperatorList.Add(NotEqualROperator);
  NotEqualROperator.InputDataCount := 2;
  NotEqualROperator.BFunctionAddr := @_NotEqualR;
  NotEqualROperator.Name := '<>';
  NotEqualROperator.Prototype := StrLogical+'<>';
  NotEqualROperator.InputDataTypes[0] := rdtDouble;
  NotEqualROperator.InputDataTypes[1] := rdtDouble;
  NotEqualROperator.OptionalArguments := 0;

  NotEqualSOperator := TFunctionClass.Create;
  OperatorList.Add(NotEqualSOperator);
  NotEqualSOperator.InputDataCount := 2;
  NotEqualSOperator.BFunctionAddr := @_NotEqualS;
  NotEqualSOperator.Name := '<>';
  NotEqualSOperator.Prototype := StrLogical+'<>';
  NotEqualSOperator.InputDataTypes[0] := rdtString;
  NotEqualSOperator.InputDataTypes[1] := rdtString;
  NotEqualSOperator.OptionalArguments := 0;

  NotEqualBOperator := TFunctionClass.Create;
  OperatorList.Add(NotEqualBOperator);
  NotEqualBOperator.InputDataCount := 2;
  NotEqualBOperator.BFunctionAddr := @_NotEqualB;
  NotEqualBOperator.Name := '<>';
  NotEqualBOperator.Prototype := StrLogical+'<>';
  NotEqualBOperator.InputDataTypes[0] := rdtBoolean;
  NotEqualBOperator.InputDataTypes[1] := rdtBoolean;
  NotEqualBOperator.OptionalArguments := 0;

  LessThanIOperator := TFunctionClass.Create;
  OperatorList.Add(LessThanIOperator);
  LessThanIOperator.InputDataCount := 2;
  LessThanIOperator.BFunctionAddr := @_LessThanI;
  LessThanIOperator.Name := '<';
  LessThanIOperator.Prototype := StrLogical+'<';
  LessThanIOperator.InputDataTypes[0] := rdtInteger;
  LessThanIOperator.InputDataTypes[1] := rdtInteger;
  LessThanIOperator.OptionalArguments := 0;

  LessThanROperator := TFunctionClass.Create;
  OperatorList.Add(LessThanROperator);
  LessThanROperator.InputDataCount := 2;
  LessThanROperator.BFunctionAddr := @_LessThanR;
  LessThanROperator.Name := '<';
  LessThanROperator.Prototype := StrLogical+'<';
  LessThanROperator.InputDataTypes[0] := rdtDouble;
  LessThanROperator.InputDataTypes[1] := rdtDouble;
  LessThanROperator.OptionalArguments := 0;

  LessThanSOperator := TFunctionClass.Create;
  OperatorList.Add(LessThanSOperator);
  LessThanSOperator.InputDataCount := 2;
  LessThanSOperator.BFunctionAddr := @_LessThanS;
  LessThanSOperator.Name := '<';
  LessThanSOperator.Prototype := StrLogical+'<';
  LessThanSOperator.InputDataTypes[0] := rdtString;
  LessThanSOperator.InputDataTypes[1] := rdtString;
  LessThanSOperator.OptionalArguments := 0;

  LessThanBOperator := TFunctionClass.Create;
  OperatorList.Add(LessThanBOperator);
  LessThanBOperator.InputDataCount := 2;
  LessThanBOperator.BFunctionAddr := @_LessThanB;
  LessThanBOperator.Name := '<';
  LessThanBOperator.Prototype := StrLogical+'<';
  LessThanBOperator.InputDataTypes[0] := rdtBoolean;
  LessThanBOperator.InputDataTypes[1] := rdtBoolean;
  LessThanBOperator.OptionalArguments := 0;

  GreaterThanIOperator := TFunctionClass.Create;
  OperatorList.Add(GreaterThanIOperator);
  GreaterThanIOperator.InputDataCount := 2;
  GreaterThanIOperator.BFunctionAddr := @_GreaterThanI;
  GreaterThanIOperator.Name := '>';
  GreaterThanIOperator.Prototype := StrLogical+'>';
  GreaterThanIOperator.InputDataTypes[0] := rdtInteger;
  GreaterThanIOperator.InputDataTypes[1] := rdtInteger;
  GreaterThanIOperator.OptionalArguments := 0;

  GreaterThanROperator := TFunctionClass.Create;
  OperatorList.Add(GreaterThanROperator);
  GreaterThanROperator.InputDataCount := 2;
  GreaterThanROperator.BFunctionAddr := @_GreaterThanR;
  GreaterThanROperator.Name := '>';
  GreaterThanROperator.Prototype := StrLogical+'>';
  GreaterThanROperator.InputDataTypes[0] := rdtDouble;
  GreaterThanROperator.InputDataTypes[1] := rdtDouble;
  GreaterThanROperator.OptionalArguments := 0;

  GreaterThanSOperator := TFunctionClass.Create;
  OperatorList.Add(GreaterThanSOperator);
  GreaterThanSOperator.InputDataCount := 2;
  GreaterThanSOperator.BFunctionAddr := @_GreaterThanS;
  GreaterThanSOperator.Name := '>';
  GreaterThanSOperator.Prototype := StrLogical+'>';
  GreaterThanSOperator.InputDataTypes[0] := rdtString;
  GreaterThanSOperator.InputDataTypes[1] := rdtString;
  GreaterThanSOperator.OptionalArguments := 0;

  GreaterThanBOperator := TFunctionClass.Create;
  OperatorList.Add(GreaterThanBOperator);
  GreaterThanBOperator.InputDataCount := 2;
  GreaterThanBOperator.BFunctionAddr := @_GreaterThanB;
  GreaterThanBOperator.Name := '>';
  GreaterThanBOperator.Prototype := StrLogical+'>';
  GreaterThanBOperator.InputDataTypes[0] := rdtBoolean;
  GreaterThanBOperator.InputDataTypes[1] := rdtBoolean;
  GreaterThanBOperator.OptionalArguments := 0;

  LessThanOrEqualsIOperator := TFunctionClass.Create;
  OperatorList.Add(LessThanOrEqualsIOperator);
  LessThanOrEqualsIOperator.InputDataCount := 2;
  LessThanOrEqualsIOperator.BFunctionAddr := @_LessThanOrEqualsI;
  LessThanOrEqualsIOperator.Name := '<=';
  LessThanOrEqualsIOperator.Prototype := StrLogical+'<=';
  LessThanOrEqualsIOperator.InputDataTypes[0] := rdtInteger;
  LessThanOrEqualsIOperator.InputDataTypes[1] := rdtInteger;
  LessThanOrEqualsIOperator.OptionalArguments := 0;

  LessThanOrEqualsROperator := TFunctionClass.Create;
  OperatorList.Add(LessThanOrEqualsROperator);
  LessThanOrEqualsROperator.InputDataCount := 2;
  LessThanOrEqualsROperator.BFunctionAddr := @_LessThanOrEqualsR;
  LessThanOrEqualsROperator.Name := '<=';
  LessThanOrEqualsROperator.Prototype := StrLogical+'<=';
  LessThanOrEqualsROperator.InputDataTypes[0] := rdtDouble;
  LessThanOrEqualsROperator.InputDataTypes[1] := rdtDouble;
  LessThanOrEqualsROperator.OptionalArguments := 0;

  LessThanOrEqualsSOperator := TFunctionClass.Create;
  OperatorList.Add(LessThanOrEqualsSOperator);
  LessThanOrEqualsSOperator.InputDataCount := 2;
  LessThanOrEqualsSOperator.BFunctionAddr := @_LessThanOrEqualsS;
  LessThanOrEqualsSOperator.Name := '<=';
  LessThanOrEqualsSOperator.Prototype := StrLogical+'<=';
  LessThanOrEqualsSOperator.InputDataTypes[0] := rdtString;
  LessThanOrEqualsSOperator.InputDataTypes[1] := rdtString;
  LessThanOrEqualsSOperator.OptionalArguments := 0;

  LessThanOrEqualsBOperator := TFunctionClass.Create;
  OperatorList.Add(LessThanOrEqualsBOperator);
  LessThanOrEqualsBOperator.InputDataCount := 2;
  LessThanOrEqualsBOperator.BFunctionAddr := @_LessThanOrEqualsB;
  LessThanOrEqualsBOperator.Name := '<=';
  LessThanOrEqualsBOperator.Prototype := StrLogical+'<=';
  LessThanOrEqualsBOperator.InputDataTypes[0] := rdtBoolean;
  LessThanOrEqualsBOperator.InputDataTypes[1] := rdtBoolean;
  LessThanOrEqualsBOperator.OptionalArguments := 0;

  GreaterThanOrEqualsIOperator := TFunctionClass.Create;
  OperatorList.Add(GreaterThanOrEqualsIOperator);
  GreaterThanOrEqualsIOperator.InputDataCount := 2;
  GreaterThanOrEqualsIOperator.BFunctionAddr := @_GreaterThanOrEqualsI;
  GreaterThanOrEqualsIOperator.Name := '>=';
  GreaterThanOrEqualsIOperator.Prototype := StrLogical+'>=';
  GreaterThanOrEqualsIOperator.InputDataTypes[0] := rdtInteger;
  GreaterThanOrEqualsIOperator.InputDataTypes[1] := rdtInteger;
  GreaterThanOrEqualsIOperator.OptionalArguments := 0;

  GreaterThanOrEqualsROperator := TFunctionClass.Create;
  OperatorList.Add(GreaterThanOrEqualsROperator);
  GreaterThanOrEqualsROperator.InputDataCount := 2;
  GreaterThanOrEqualsROperator.BFunctionAddr := @_GreaterThanOrEqualsR;
  GreaterThanOrEqualsROperator.Name := '>=';
  GreaterThanOrEqualsROperator.Prototype := StrLogical+'>=';
  GreaterThanOrEqualsROperator.InputDataTypes[0] := rdtDouble;
  GreaterThanOrEqualsROperator.InputDataTypes[1] := rdtDouble;
  GreaterThanOrEqualsROperator.OptionalArguments := 0;

  GreaterThanOrEqualsSOperator := TFunctionClass.Create;
  OperatorList.Add(GreaterThanOrEqualsSOperator);
  GreaterThanOrEqualsSOperator.InputDataCount := 2;
  GreaterThanOrEqualsSOperator.BFunctionAddr := @_GreaterThanOrEqualsS;
  GreaterThanOrEqualsSOperator.Name := '>=';
  GreaterThanOrEqualsSOperator.Prototype := StrLogical+'>=';
  GreaterThanOrEqualsSOperator.InputDataTypes[0] := rdtString;
  GreaterThanOrEqualsSOperator.InputDataTypes[1] := rdtString;
  GreaterThanOrEqualsSOperator.OptionalArguments := 0;

  GreaterThanOrEqualsBOperator := TFunctionClass.Create;
  OperatorList.Add(GreaterThanOrEqualsBOperator);
  GreaterThanOrEqualsBOperator.InputDataCount := 2;
  GreaterThanOrEqualsBOperator.BFunctionAddr := @_GreaterThanOrEqualsB;
  GreaterThanOrEqualsBOperator.Name := '>=';
  GreaterThanOrEqualsBOperator.Prototype := StrLogical+'>=';
  GreaterThanOrEqualsBOperator.InputDataTypes[0] := rdtBoolean;
  GreaterThanOrEqualsBOperator.InputDataTypes[1] := rdtBoolean;
  GreaterThanOrEqualsBOperator.OptionalArguments := 0;

  OverloadedFunctionList := TObjectList.Create;

  AbsIOverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(AbsIOverloadedFunction);
  AbsIOverloadedFunction.InputDataCount := 1;
  AbsIOverloadedFunction.IFunctionAddr := @_AbsI;
  AbsIOverloadedFunction.Name := 'Abs';
  AbsIOverloadedFunction.Prototype := StrMath+'Abs(Value)';
  AbsIOverloadedFunction.InputDataTypes[0] := rdtInteger;
  AbsIOverloadedFunction.OptionalArguments := 0;

  AbsROverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(AbsROverloadedFunction);
  AbsROverloadedFunction.InputDataCount := 1;
  AbsROverloadedFunction.RFunctionAddr := @_AbsR;
  AbsROverloadedFunction.Name := 'Abs';
  AbsROverloadedFunction.Prototype := StrMath+'Abs(Value)';
  AbsROverloadedFunction.InputDataTypes[0] := rdtDouble;
  AbsROverloadedFunction.OptionalArguments := 0;

  MaxIOverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(MaxIOverloadedFunction);
  MaxIOverloadedFunction.InputDataCount := 3;
  MaxIOverloadedFunction.OptionalArguments := -1;
  MaxIOverloadedFunction.IFunctionAddr := @_MaxI;
  MaxIOverloadedFunction.Name := 'Max';
  MaxIOverloadedFunction.Prototype := StrMath+'Max(Value1, Value2, ...)';
  MaxIOverloadedFunction.InputDataTypes[0] := rdtInteger;
  MaxIOverloadedFunction.InputDataTypes[1] := rdtInteger;
  MaxIOverloadedFunction.InputDataTypes[2] := rdtInteger;

  MaxROverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(MaxROverloadedFunction);
  MaxROverloadedFunction.InputDataCount := 3;
  MaxROverloadedFunction.OptionalArguments := -1;
  MaxROverloadedFunction.RFunctionAddr := @_MaxR;
  MaxROverloadedFunction.Name := 'Max';
  MaxROverloadedFunction.Prototype := StrMath+'Max(Value1, Value2, ...)';
  MaxROverloadedFunction.InputDataTypes[0] := rdtDouble;
  MaxROverloadedFunction.InputDataTypes[1] := rdtDouble;
  MaxROverloadedFunction.InputDataTypes[2] := rdtDouble;

  MinIOverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(MinIOverloadedFunction);
  MinIOverloadedFunction.InputDataCount := 3;
  MinIOverloadedFunction.OptionalArguments := -1;
  MinIOverloadedFunction.IFunctionAddr := @_MinI;
  MinIOverloadedFunction.Name := 'Min';
  MinIOverloadedFunction.Prototype := StrMath+'Min(Value1, Value2, ...)';
  MinIOverloadedFunction.InputDataTypes[0] := rdtInteger;
  MinIOverloadedFunction.InputDataTypes[1] := rdtInteger;
  MinIOverloadedFunction.InputDataTypes[2] := rdtInteger;

  MinROverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(MinROverloadedFunction);
  MinROverloadedFunction.InputDataCount := 3;
  MinROverloadedFunction.OptionalArguments := -1;
  MinROverloadedFunction.RFunctionAddr := @_MinR;
  MinROverloadedFunction.Name := 'Min';
  MinROverloadedFunction.Prototype := StrMath+'Min(Value1, Value2, ...)';
  MinROverloadedFunction.InputDataTypes[0] := rdtDouble;
  MinROverloadedFunction.InputDataTypes[1] := rdtDouble;
  MinROverloadedFunction.InputDataTypes[2] := rdtDouble;

  SqrIOverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(SqrIOverloadedFunction);
  SqrIOverloadedFunction.InputDataCount := 1;
  SqrIOverloadedFunction.OptionalArguments := 0;
  SqrIOverloadedFunction.IFunctionAddr := @_SqrI;
  SqrIOverloadedFunction.Name := 'Sqr';
  SqrIOverloadedFunction.Prototype := StrMath+'Sqr(Value)';
  SqrIOverloadedFunction.InputDataTypes[0] := rdtInteger;

  SqrROverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(SqrROverloadedFunction);
  SqrROverloadedFunction.InputDataCount := 1;
  SqrROverloadedFunction.OptionalArguments := 0;
  SqrROverloadedFunction.RFunctionAddr := @_SqrR;
  SqrROverloadedFunction.Name := 'Sqr';
  SqrROverloadedFunction.Prototype := StrMath+'Sqr(Value)';
  SqrROverloadedFunction.InputDataTypes[0] := rdtDouble;

  CaseBOverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(CaseBOverloadedFunction);
  CaseBOverloadedFunction.InputDataCount := 4;
  CaseBOverloadedFunction.OptionalArguments := -1;
  CaseBOverloadedFunction.BFunctionAddr := @_CaseBoolean;
  CaseBOverloadedFunction.Name := 'Case';
  CaseBOverloadedFunction.Prototype :=
    StrLogical+'Case(Index, Result1, Result2, ...)';
  CaseBOverloadedFunction.InputDataTypes[0] := rdtInteger;
  CaseBOverloadedFunction.InputDataTypes[1] := rdtBoolean;
  CaseBOverloadedFunction.InputDataTypes[2] := rdtBoolean;
  CaseBOverloadedFunction.InputDataTypes[3] := rdtBoolean;

  CaseIOverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(CaseIOverloadedFunction);
  CaseIOverloadedFunction.InputDataCount := 4;
  CaseIOverloadedFunction.OptionalArguments := -1;
  CaseIOverloadedFunction.IFunctionAddr := @_CaseInteger;
  CaseIOverloadedFunction.Name := 'Case';
  CaseIOverloadedFunction.Prototype :=
    StrLogical+'Case(Index, Result1, Result2, ...)';
  CaseIOverloadedFunction.InputDataTypes[0] := rdtInteger;
  CaseIOverloadedFunction.InputDataTypes[1] := rdtInteger;
  CaseIOverloadedFunction.InputDataTypes[2] := rdtInteger;
  CaseIOverloadedFunction.InputDataTypes[3] := rdtInteger;

  CaseROverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(CaseROverloadedFunction);
  CaseROverloadedFunction.InputDataCount := 4;
  CaseROverloadedFunction.OptionalArguments := -1;
  CaseROverloadedFunction.RFunctionAddr := @_CaseDouble;
  CaseROverloadedFunction.Name := 'Case';
  CaseROverloadedFunction.Prototype :=
    StrLogical+'Case(Index, Result1, Result2, ...)';
  CaseROverloadedFunction.InputDataTypes[0] := rdtInteger;
  CaseROverloadedFunction.InputDataTypes[1] := rdtDouble;
  CaseROverloadedFunction.InputDataTypes[2] := rdtDouble;
  CaseROverloadedFunction.InputDataTypes[3] := rdtDouble;

  CaseSOverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(CaseSOverloadedFunction);
  CaseSOverloadedFunction.InputDataCount := 4;
  CaseSOverloadedFunction.OptionalArguments := -1;
  CaseSOverloadedFunction.SFunctionAddr := @_CaseString;
  CaseSOverloadedFunction.Name := 'Case';
  CaseSOverloadedFunction.Prototype :=
    StrLogical+'Case(Index, Result1, Result2, ...)';
  CaseSOverloadedFunction.InputDataTypes[0] := rdtInteger;
  CaseSOverloadedFunction.InputDataTypes[1] := rdtString;
  CaseSOverloadedFunction.InputDataTypes[2] := rdtString;
  CaseSOverloadedFunction.InputDataTypes[3] := rdtString;

  IfBOverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(IfBOverloadedFunction);
  IfBOverloadedFunction.InputDataCount := 3;
  IfBOverloadedFunction.OptionalArguments := 0;
  IfBOverloadedFunction.BFunctionAddr := @_IfBoolean;
  IfBOverloadedFunction.Name := 'If';
  IfBOverloadedFunction.Prototype :=
    StrLogical+'If(Boolean_Value, If_True_Result, If_False_Result)';
  IfBOverloadedFunction.InputDataTypes[0] := rdtBoolean;
  IfBOverloadedFunction.InputDataTypes[1] := rdtBoolean;
  IfBOverloadedFunction.InputDataTypes[2] := rdtBoolean;

  IfIOverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(IfIOverloadedFunction);
  IfIOverloadedFunction.InputDataCount := 3;
  IfIOverloadedFunction.OptionalArguments := 0;
  IfIOverloadedFunction.IFunctionAddr := @_IfInteger;
  IfIOverloadedFunction.Name := 'If';
  IfIOverloadedFunction.Prototype :=
    StrLogical+'If(Boolean_Value, If_True_Result, If_False_Result)';
  IfIOverloadedFunction.InputDataTypes[0] := rdtBoolean;
  IfIOverloadedFunction.InputDataTypes[1] := rdtInteger;
  IfIOverloadedFunction.InputDataTypes[2] := rdtInteger;

  IfROverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(IfROverloadedFunction);
  IfROverloadedFunction.InputDataCount := 3;
  IfROverloadedFunction.OptionalArguments := 0;
  IfROverloadedFunction.RFunctionAddr := @_IfDouble;
  IfROverloadedFunction.Name := 'If';
  IfROverloadedFunction.Prototype :=
    StrLogical+'If(Boolean_Value, If_True_Result, If_False_Result)';
  IfROverloadedFunction.InputDataTypes[0] := rdtBoolean;
  IfROverloadedFunction.InputDataTypes[1] := rdtDouble;
  IfROverloadedFunction.InputDataTypes[2] := rdtDouble;

  IfSOverloadedFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(IfSOverloadedFunction);
  IfSOverloadedFunction.InputDataCount := 3;
  IfSOverloadedFunction.OptionalArguments := 0;
  IfSOverloadedFunction.SFunctionAddr := @_IfString;
  IfSOverloadedFunction.Name := 'If';
  IfSOverloadedFunction.Prototype :=
    StrLogical+'If(Boolean_Value, If_True_Result, If_False_Result)';
  IfSOverloadedFunction.InputDataTypes[0] := rdtBoolean;
  IfSOverloadedFunction.InputDataTypes[1] := rdtString;
  IfSOverloadedFunction.InputDataTypes[2] := rdtString;

  MultiInterpolateFunction := TFunctionClass.Create;
  OverloadedFunctionList.Add(MultiInterpolateFunction);
  MultiInterpolateFunction.InputDataCount := 4;
  MultiInterpolateFunction.OptionalArguments := -1;
  MultiInterpolateFunction.RFunctionAddr := @_MultiInterpolate;
  MultiInterpolateFunction.Name := 'MultiInterpolate';
  MultiInterpolateFunction.Prototype :=
    StrMath+'MultiInterpolate(Position, Value1, Distance1, [Value2, Distance2,] ...)';
  MultiInterpolateFunction.InputDataTypes[0] := rdtDouble;
  MultiInterpolateFunction.InputDataTypes[1] := rdtDouble;
  MultiInterpolateFunction.InputDataTypes[2] := rdtDouble;
  MultiInterpolateFunction.InputDataTypes[3] := rdtDouble;
end;

procedure TIntToDoubleExpression.MakeDiagram(List: TStringList; Level: integer);
var
  ArrayLength: integer;
  AVariable: TConstant;
begin
  ArrayLength := Length(Data);
  Assert(ArrayLength = 1);
  AVariable := TConstant(Data[0].Datum);
  Assert(AVariable <> nil);
  AVariable.MakeDiagram(List, Level);
end;

{ TVariableExpression }

constructor TVariableExpression.Create(const Variable: TCustomValue;
  SpecialImplementorList: TSpecialImplementorList);
var
  VariableConverter: TFunctionRecord;
begin
  VariableConverter.ResultType := Variable.ResultType;
  VariableConverter.OptionalArguments := 0;
  VariableConverter.CanConvertToConstant := False;
  VariableConverter.Prototype := 'Dummy';
  VariableConverter.RFunctionAddr := nil;
  VariableConverter.Name := Variable.FUserName;
  SetLength(VariableConverter.InputDataTypes, 1);
  VariableConverter.InputDataTypes[0] := Variable.ResultType;
  inherited Create(VariableConverter, SpecialImplementorList);
  Variables[0] := Variable;
end;

function TVariableExpression.Decompile: string;
begin
  result := DecompileByType(dtInternal);
end;

function TVariableExpression.DecompileByType(DecompileType: TDecompileType): string;
var
  AVariable: TConstant;
begin
  Assert(Length(Data) = 1);
  AVariable := TConstant(Data[0].Datum);
  Assert(AVariable <> nil);
  result := '';
  case DecompileType of
    dtInternal: result := AVariable.Decompile;
    dtDisplay: result := AVariable.DecompileDisplay;
    else Assert(False);
  end;
end;

function TVariableExpression.DecompileDisplay: string;
begin
  result := DecompileByType(dtDisplay);
end;

procedure TVariableExpression.Evaluate;
var
  resultVariable: TConstant;
begin
  resultVariable := Variables[0];
  assert(resultVariable.ResultType = ResultType);
  SetResultFromConstant(resultVariable, self);

end;

procedure TVariableExpression.MakeDiagram(List: TStringList; Level: integer);
var
  Line: string;
  Index: Integer;
begin
  Line := '';
  for Index := 0 to Level - 1 do
  begin
    Line := Line + #9;
  end;
  Line := Line + FUserName + #9 + DecompileDisplay;
  List.Add(Line);
end;

{ TMultiInterpolateExpression }

procedure TMultiInterpolateExpression.Evaluate;
var
  ArrayLength: integer;
  AVariable: TConstant;
  PositionVariable: TConstant;
  Position: double;
  PriorDistance: double;
  DistanceIndex: integer;
  Distance: double;
  PriorValue: double;
  Value: double;
begin
  if ShouldEvaluate then
  begin
    ArrayLength := Length(Data);
    Assert(ArrayLength >= 3);
    Assert((ArrayLength mod 2) = 1);

    if ArrayLength = 3 then
    begin
      AVariable := TConstant(Data[1].Datum);
      if AVariable is TExpression then
      begin
        TExpression(AVariable).Evaluate;
      end;
      SetResultFromNumber(AVariable.DoubleResult);
      Exit;
    end;

    Assert(Data[0].DataType in [rdtDouble, rdtInteger]);
    PositionVariable := TConstant(Data[0].Datum);
    if PositionVariable is TExpression then
    begin
      TExpression(PositionVariable).Evaluate;
    end;
    Position := PositionVariable.DoubleResult;

    PriorDistance := 0;
    for DistanceIndex := 0 to ArrayLength div 2 - 1 do
    begin
      Assert(Data[DistanceIndex * 2 + 2].DataType in [rdtDouble, rdtInteger]);
      AVariable := TConstant(Data[DistanceIndex * 2 + 2].Datum);
      if AVariable is TExpression then
      begin
        TExpression(AVariable).Evaluate;
      end;
      Distance := AVariable.DoubleResult;
      if Position < Distance then
      begin
        Assert(Data[DistanceIndex * 2 + 1].DataType in [rdtDouble, rdtInteger]);
        AVariable := TConstant(Data[DistanceIndex * 2 + 1].Datum);
        if AVariable is TExpression then
        begin
          TExpression(AVariable).Evaluate;
        end;
        Value := AVariable.DoubleResult;

        if (DistanceIndex = 0) or (Distance = PriorDistance) then
        begin
          SetResultFromNumber(Value);
//          PDouble(FResult)^ := Value;
        end
        else
        begin
          Assert(Data[DistanceIndex * 2 - 1].DataType in [rdtDouble,
            rdtInteger]);
          AVariable := TConstant(Data[DistanceIndex * 2 - 1].Datum);
          if AVariable is TExpression then
          begin
            TExpression(AVariable).Evaluate;
          end;
          PriorValue := AVariable.DoubleResult;

          SetResultFromNumber((Position - PriorDistance)
            / (Distance - PriorDistance) * (Value - PriorValue) + PriorValue);
//          PDouble(FResult)^ := (Position - PriorDistance)
//            / (Distance - PriorDistance) * (Value - PriorValue) + PriorValue;
        end;
        Exit;
      end
      else if Distance = Position then
      begin
        Assert(Data[DistanceIndex * 2 + 1].DataType in [rdtDouble, rdtInteger]);
        AVariable := TConstant(Data[DistanceIndex * 2 + 1].Datum);
        if AVariable is TExpression then
        begin
          TExpression(AVariable).Evaluate;
        end;
        SetResultFromNumber(AVariable.DoubleResult);
//        PDouble(FResult)^ := AVariable.DoubleResult;
        Exit;
      end
      else
      begin
        if DistanceIndex = (ArrayLength div 2) then
        begin
          Assert(Data[DistanceIndex * 2 + 1].DataType in [rdtDouble,
            rdtInteger]);
          AVariable := TConstant(Data[DistanceIndex * 2 + 1].Datum);
          if AVariable is TExpression then
          begin
            TExpression(AVariable).Evaluate;
          end;
          SetResultFromNumber(AVariable.DoubleResult);
//          PDouble(FResult)^ := AVariable.DoubleResult;
          Exit;
        end;
        PriorDistance := Distance;
      end;
    end;
  end;
end;

{$WARNINGS OFF}
procedure TMultiInterpolateExpression.SetResultFromNumber(ANumber: Double);
begin
  PDouble(FResult)^ := ANumber;
end;
{$WARNINGS ON}

{ TSpecialImplementorList }

function TSpecialImplementorList.Add(
  const Item: TSpecialImplementor): Integer;
begin
  result := FList.Add(Item);
end;

procedure TSpecialImplementorList.Clear;
begin
  FList.Clear
end;

constructor TSpecialImplementorList.Create;
begin
  inherited;
  FList := TList.Create;
end;

procedure TSpecialImplementorList.Delete(const Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TSpecialImplementorList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TSpecialImplementorList.GetCapacity: integer;
begin
  result := FList.Capacity;
end;

function TSpecialImplementorList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSpecialImplementorList.GetItems(
  const Index: integer): TSpecialImplementor;
begin
  result := TSpecialImplementor(FList[Index]);
end;

function TSpecialImplementorList.Remove(
  const Item: TSpecialImplementor): Integer;
begin
  result := FList.Remove(Item)
end;

procedure TSpecialImplementorList.SetCapacity(const Value: integer);
begin
  FList.Capacity := Value;
end;

procedure TSpecialImplementorList.SetItems(const Index: integer;
  const Value: TSpecialImplementor);
begin
  FList[Index] := Value;
end;

{ ERbwParserError }

constructor ERbwParserError.CreateMode(const Msg: string;
  const ErrorMode: integer);
begin
  inherited Create(Msg);
  ErrorType := ErrorMode
end;

{ TOperatorDefinition }

constructor TOperatorDefinition.Create;
begin
  inherited;
  FArgumentDefinitions:= TArgumentList.Create;
end;

destructor TOperatorDefinition.Destroy;
begin
  FArgumentDefinitions.Free;
  inherited;
end;

{ TNotifierComponent }

constructor TNotifierComponent.Create(AnOwner: TExpression);
begin
  inherited Create(nil);
  FExpression := AnOwner;
end;

initialization
  begin
    InitializeVariables
  end;

finalization
  OperatorList.Free;
  OverloadedFunctionList.Free;
  IntToDoubleFunction.Free;
end.


