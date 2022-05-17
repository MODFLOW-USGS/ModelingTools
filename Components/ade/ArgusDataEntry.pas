{
			    User Rights Notice

This software and related material (data and documentation), contained in or
furnished in connection with TArgusDataEntry component are made available 
by the U.S. Geological Survey (USGS) to be used in
the public interest and the advancement of science.  You may, without any fee
or cost, use, copy, modify, or distribute this software, and any derivative
works thereof, and its supporting documentation, subject to the following
restrictions and understandings.

If you distribute copies or modifications of the software and related
material, make sure the recipients receive a copy of this notice and receive
or can get a copy of the original distribution.  If the software and (or)
related material are modified and distributed, it must be made clear that the
recipients do not have the original and are informed of the extent of the
modifications.  For example, modified files must include a prominent notice
stating the modifications, author, and date.  This restriction is necessary to
guard against problems introduced in the software by others, reflecting
negatively on the reputation of the USGS.

The software is public property, you therefore have the right to the source
code, if desired.

You may charge fees for distribution, warranties, and services provided in
connection with the software or derivative works thereof.  The name USGS can
be used in any advertising or publicity to endorse or promote any products or
commercial entity using this software if specific written permission is
obtained from the USGS.

The user agrees to appropriately acknowledge the authors and the USGS in
publications that result from the use of this software or in products that
include this software in whole or in part.

Because the software and related material is free (other than nominal
materials and handling fees) and provided "as is", the authors, USGS, or the
United States Government have made no warranty, expressed or implied, as to
the accuracy or completeness and are not obligated to provide the user with
any support, consulting, training or assistance of any kind with regard to the
use, operation, and performance of this software nor to provide the user with
any updates, revisions, new versions or "bug fixes".

The user assumes all risk for any damages whatsoever resulting from loss of
use, data, or profits arising in connection with the access, use, quality, or
performance of this software.
}

unit ArgusDataEntry;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AppEvnts;

{$ifdef CONDITIONALEXPRESSIONS}
  {$if CompilerVersion>=20}
    {$DEFINE Delphi_2009_UP}
  {$ifend}
{$endif}

type

  TDataType = (dtCustom, dtString, dtInteger, dtReal, dtBoolean);

  EArgusMinMaxError = class(Exception);
  {EArgusMinMaxError is raised if there is an attempt to set the Max to a
   value less than Min or vice versa.}

  TArgusDataEntry = class(TComboBox)
    {TArgusDataEntry provides data checking for user entered data. All data
    entered in a TArgusDataEntry is first checked to be sure it can be converted
    to another type of data as specified by the DataType property. In the event
    that the data can not be converted, characters will be stripped from the
    end of Text until the conversion is possible. If the DataType is dtInteger
    or dtReal, it is also possible to check that the numeric representation
    of Text does not lie outside the range Min..Max. Use CheckMin and CheckMax
    to turn on or off range checking. OnExceedingBounds and OnExceededBounds can
    be used to provide custom handling for cases where the data do lie outside
    the range specified by Min..Max when range checking is on.}
  private
    { Private declarations }
    FOutput : string;
    FDataType : TDataType;
    FMax : extended;
    FMin : extended;
    FCheckMax : boolean;
    FCheckMin : boolean;
    FOnStyleChanged : TNotifyEvent;
    FOnDataTypeChanged : TNotifyEvent;
    FOnExceededBounds : TNotifyEvent;
    FOnExceedingBounds : TNotifyEvent;
    FStyle : TComboBoxStyle;
    FChangeDisabledColor: boolean;
    FDisabledColor: TColor;
    FEnabledColor: TColor;
    FDecimalDelimiter: Char;
    FAppEvents: TApplicationEvents;
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
//    function GetDecChar : char;
    procedure LocalizeString(var ANumberString : string);
    function ReadOutput : string ;
    function LocalStrToFloat(const S: string): Extended;
    procedure SetChangeDisabledColor(const Value: boolean);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetEnabledColor(Value: TColor);
    function GetRealValue: Double;
    procedure SetRealValue(const Value: Double);
    function GetIntegerValue: Integer;
    procedure SetIntegerValue(const Value: Integer);
  protected
    { Protected declarations }
 {   function GetText: TCaption;

    procedure SetText(const Value: TCaption);     }

    procedure SetDataType(ADataType : TDataType); virtual;
      {SetDataType sets the DataType property. It also changes the Style of the
       TArgusDataEntry depending on the Data type. dtString, dtInteger, and
       dtReal result in the style being set to csSimple. dtBoolean
       results in the Style being set to csDropDownList. If the data type is set
       to dtCustom, an Style is allowed. It also changes the text to make sure
       it is consistent with the data type and updates Output to be consistent
       with Text. Items[0] and Items[1] are reset to 'False' and 'True'
       respectively every time the data type is set to dtBoolean. If the new
       DataType is different from the existing DataType, an OnChangeDataType
       event occurs.}
    procedure SetEnabled(Value: Boolean); override;
      {SetEnabled calls the inherited SetEnabled and then ChangeColor}

    procedure SetStyle(Value: TComboBoxStyle); override;
      {SetStyle changes to Style of the TArgusDataEntry and checks that the style is
       consistent with the DataType. If the new
       Style is different from the existing Style, an OnChangeStyle
       event occurs.}

    procedure SetMax(Value : extended); virtual;
       {SetMax changes Max and then calls CheckRange}

    procedure SetMin(Value : extended); virtual;
       {SetMin changes Min and then calls CheckRange}

    procedure SetCheckMax(Value : boolean); virtual;
       {SetCheckMax changes CheckMax and then if CheckMax is true calls CheckRange}

    procedure SetCheckMin(Value : boolean); virtual;
       {SetCheckMin changes CheckMin and then if CheckMin is true calls CheckRange}

    procedure DoExit ; override;
      {DoExit implements the OnExit event; It calls the inherited method and then CheckRange}

    procedure Change; override;
      {Change implements the OnChange event; It calls the inherited method and then TextChanged.}

    procedure StyleChanged; dynamic;
      {StyleChanged implements the OnStyleChanged event; It is called whenever
      the application changes the data type.}

    procedure DataTypeChanged; dynamic;
      {DataTypeChanged implements the OnDataTypeChanged event;}

    procedure ExceedingBounds; dynamic;
      {ExceedingBounds implements the OnExceedingBounds event;}

    procedure ExceededBounds; dynamic;
      {StyleChanged implements the OnStyleChanged event;}

    procedure TextChanged; virtual;
      {TextChanged is called before the event handler for the OnChanged event.
      It checks that the data entered in the TArgusDataEntry is valid. It does
      not reject '-' even though that is not a valid integer or real number.
      However, if the text is '-' when OnExit occurs, the CheckRange procedure
      will catch the error.}

    procedure ChangeColor;
    {ChangeColor changes the color of the control depending on whether the
     control is enabled or not and the ChangeDisabledColor property.}
     procedure SettingsChanged(Sender: TObject; Flag: Integer; const Section: string; var Result: Longint);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    {Create creates and initializes a TArgusDataEntry instance.}

    function ChangeTextToInteger : String; virtual;
      {ChangeTextToInteger takes the string in the Text property and attempts to
      convert it to an integer. If it fails, it strips one character at a time
      from the end of Text until it does succeed. It converts '' to '0'. The
      end result is a string that can be converted to an integer.}

    function ChangeTextToReal : string; virtual;
      {ChangeTextToReal takes the string in the Text property and attempts to
      convert it to an extended real number. If it fails, it strips one character at a time
      from the end of Text until it does succeed. It converts '' to '0'.  The
      end result is a string that can be converted to an real number.}

    function ChangeTextToBoolean : string; virtual;
      {ChangeTextToBoolean converts the string in the Text property to '0' if
       the Text is '', '0' or 'false'. The check that the Text is equal to
      'false' is case insensitive.}

    procedure CheckRange; virtual;
      {CheckRange is called before the event handler for the OnExit event.
      It checks that the data in the TArgusDataEntry do not exceed the
      specified limits. However, it only checks the range if the DataType is
      dtInteger or dtReal. It also catches the invalid value of '-' for real
      and integer data typed in the text field. If the value in the data field
      should be checked as specified by CheckMin and CheckMax and is
      outside the limits set by Min and Max an OnExceedingBounds event occurs.
      If after the eventhandler for OnExceedingBounds, the numeric
      representation of Text is still outside the appropriate range, Text will
      be changed to Max or Min, whichever is closer and raises an
      OnExceededBounds event.}
    property RealValue: Double read GetRealValue write SetRealValue;
    function RealValueDefault(DefaultValue: double): double;
    property IntegerValue: Integer read GetIntegerValue write SetIntegerValue;
    function TryGetRealValue(var AValue: Double): Boolean;
  published
    { Published declarations }
    property DataType: TDataType read FDataType write SetDataType default dtString;
    {Set the DataType to an appropriate value to cause the TArgusDataEntry to
    check that user-entered data is the appropriate type.}

    property Max : extended read FMax write SetMax;
      {If CheckMax is true and the DataType is dtInteger or dtReal, the
      TArgusDataEntry will check the data entered and change it to Max if the
      entered value is larger than Max. It will also beep and set the focus to
      the TArgusDataEntry. To modify this behaviour, you can use the
      OnExceedingBounds or OnExceededBounds events.}

    property Min : extended read FMin write SetMin;
      {If CheckMin is true and the DataType is dtInteger or dtReal, the
      TArgusDataEntry will check the data entered and change it to Min if the
      entered value is smaller than Max. It will also beep and set the focus to
      the TArgusDataEntry.  To modify this behaviour, you can use the
      OnExceedingBounds or OnExceededBounds events.}

    property CheckMax: boolean read FCheckMax write SetCheckMax default False;
      {If CheckMax is true and the DataType is dtInteger or dtReal, the
      TArgusDataEntry will check the data entered and change it to Max if the
      entered value is larger than Max. It will also beep and set the focus to
      the TArgusDataEntry. To modify this behaviour, you can use the
      OnExceedingBounds or OnExceededBounds events.}

    property CheckMin: boolean read FCheckMin write SetCheckMin default False;
      {If CheckMin is true and the DataType is dtInteger or dtReal, the
      TArgusDataEntry will check the data entered and change it to Min if the
      entered value is smaller than Min. It will also beep and set the focus to
      the TArgusDataEntry. To modify this behaviour, you can use the
      OnExceedingBounds or OnExceededBounds events.}

    property Output: string read ReadOutput ;
      {If the DataType is anything other than dtBoolean, Output will be the
      same as Text. For dtBoolean, Output is '0' if Text is 'False' and '1' if
      Text is 'True'. Output is read-only. Output always uses '.' as the
      decimal separator.}

    property Style: TComboBoxStyle read FStyle write SetStyle default csSimple;
      {Style is inherited but the default is changed to csSimple.}

    Property OnStyleChanged : TNotifyEvent read FOnStyleChanged write FOnStyleChanged ;
      {Whenever the Style Property is changed, an  OnStyleChanged event occurs.}

    Property OnDataTypeChanged : TNotifyEvent read FOnDataTypeChanged write FOnDataTypeChanged ;
      {Whenever the DataType Property is changed, an  OnDataTypeChanged event occurs.}

    Property OnExceededBounds : TNotifyEvent read FOnExceededBounds write FOnExceededBounds ;
      {An OnExceededBounds event occurs if
      1. the DataType is dtInteger or dtReal and either
      2. CheckMax is true and the numeric representation of the Text Property
         exceeds the Max Property or
      3. CheckMin is true and the numeric representation of the Text Property
         is less than the Min Property.
      OnExceededBounds occurs after the default handling of such cases has
      occurred.
      CheckRange will be called again after your event handler. Be careful
      that your custom handling does not result in an infinite loop.}

    Property OnExceedingBounds : TNotifyEvent read FOnExceedingBounds write FOnExceedingBounds ;
      {An OnExceedingBounds event occurs in the CheckRange procedure if
      1. the DataType is dtInteger or dtReal and either
      2. CheckMax is true and the numeric representation of the Text Property
         exceeds the Max Property or
      3. CheckMin is true and the numeric representation of the Text Property
         is less than the Min Property.
      OnExceedingBounds provides an opportunity for custom handling of such cases
      before they have been handled automatically. CheckRange will be called
      again after your event handler. Be careful that your custom handling
      does not result in an infinite loop.}

 {   Property Text : TCaption read GetText Write SetText ; }

    property Text: TCaption read GetText write SetText;
    {Identical to original Text property except that setting the text
     generates an OnChange Event.}
    property ChangeDisabledColor : boolean read FChangeDisabledColor write SetChangeDisabledColor;
    {If ChangeDisabledColor is true, the color of the control will be changed
     to EnabledColor when the control becomes enabled and will be set to
     DisabledColor when the control becomes disabled.}
    property DisabledColor : TColor read FDisabledColor write SetDisabledColor
      default clBtnFace;
    {DisabledColor is the color that the control will be changed to if
     ChangeDisabledColor is true and the control becomes disabled.  By default,
     DisabledColor is clBtnFace.}
    property EnabledColor: TColor read FEnabledColor write SetEnabledColor
      default clWindow;
    {EnabledColor is the color that the control will be changed to if
     ChangeDisabledColor is true and the control becomes enabled.  By default,
     EnabledColor is clWindow.}

  end;

  TRbwDataEntry = class(TArgusDataEntry);

procedure Register;


implementation

var
  MaxDouble : double;
  DummyInteger : integer;

procedure Register;
begin
  RegisterComponents('RBW', [TArgusDataEntry, TRbwDataEntry]);
end;

{function TArgusDataEntry.GetDecChar : Char;
VAR
  SelectedLCID : LCID;
  AString : string;
begin
  SelectedLCID := GetUserDefaultLCID;
  AString := GetLocaleStr(SelectedLCID, LOCALE_SDECIMAL, '.');
  result := AString[1];
end;  }


procedure TArgusDataEntry.LocalizeString(var ANumberString : string);
var
  DecimalPosition : integer;
  DecimalChar : Char;
begin
  {$IFDEF Delphi_2009_UP}
  DecimalChar := FormatSettings.DecimalSeparator;
  {$ELSE}
  DecimalChar := DecimalSeparator;
  {$ENDIF}
  if (DecimalChar = '.') then
  begin
    DecimalPosition := Pos(',', ANumberString);
    if DecimalPosition > 0 then
    begin
      ANumberString[DecimalPosition] := DecimalChar;
{      ANumberString := Copy(ANumberString, 1, DecimalPosition -1) + DecimalChar
        + Copy(ANumberString, DecimalPosition + 1, Length(ANumberString));}
    end;
  end
  else
  begin
    DecimalPosition := Pos('.', ANumberString);
    if DecimalPosition > 0 then
    begin
      ANumberString[DecimalPosition] := DecimalChar;
{      ANumberString := Copy(ANumberString, 1, DecimalPosition -1) + DecimalChar
        + Copy(ANumberString, DecimalPosition + 1, Length(ANumberString));}
    end;
  end;
end;

function TArgusDataEntry.LocalStrToFloat(const S: string): Extended;
var
  DecimalPosition : integer;
  DecimalChar : Char;
  AString : string;
begin
  AString := S;
  {$IFDEF Delphi_2009_UP}
  DecimalChar := FormatSettings.DecimalSeparator;
  {$ELSE}
  DecimalChar := DecimalSeparator;
  {$ENDIF}
  if not (DecimalChar = '.') then
  begin
    DecimalPosition := Pos('.', S);
    if DecimalPosition > 0
    then
      begin
        AString[DecimalPosition] := DecimalChar;
{        AString := Copy(S, 1, DecimalPosition -1) + DecimalChar
          + Copy(S, DecimalPosition + 1, Length(S));}
      end;
  end;
  if not (DecimalChar = ',') then
  begin
    DecimalPosition := Pos(',', S);
    if DecimalPosition > 0
    then
      begin
        AString[DecimalPosition] := DecimalChar;
{        AString := Copy(S, 1, DecimalPosition -1) + DecimalChar
          + Copy(S, DecimalPosition + 1, Length(S));}
      end;
  end;
  result := StrToFloat(AString);
end;



function TArgusDataEntry.ReadOutput : string;
var
  DecimalPosition : integer;
  DecimalChar : string;
  EPos1, EPos2 : integer;
begin
  Result := Text;
  If (FDataType = dtReal) or (FDataType = dtInteger) then
  begin
    {$IFDEF Delphi_2009_UP}
    DecimalChar := FormatSettings.DecimalSeparator;
    {$ELSE}
    DecimalChar := DecimalSeparator;
    {$ENDIF}
    if (DecimalChar <> '.')
    then
      begin
        DecimalPosition := Pos(DecimalChar, result);
        if DecimalPosition > 0 then
        begin
          Result[DecimalPosition] := '.';
{          Result := Copy(result, 1, DecimalPosition -1) + '.'
            + Copy(FOutput, DecimalPosition + 1, Length(result));}
        end;
      end;
  end;
  if (FDataType = dtReal) then
  begin
    DecimalPosition := Pos('.', result);
    if (DecimalPosition = 0) then
    begin
      EPos1 := Pos('e', result);
      EPos2 := Pos('E', result);
      if (EPos1 = 0) and (EPos2 = 0) then
      begin
        result := result + '.';
      end;
    end;
  end;
end;


function TArgusDataEntry.RealValueDefault(DefaultValue: double): double;
begin
  Assert(DataType = dtReal);
  // If this is changed, change GetRealValue too.
  if (Text <> '') and (Text <> '+') and (Text <> '-') then
  begin
    result := StrToFloat(Text)
  end
  else
  begin
    Result := DefaultValue;
  end;
end;

procedure TArgusDataEntry.SetMax(Value : extended);
begin
  if FMax <> Value then
  begin
    if Value >= FMin then
   begin
     FMax := Value;
     If CheckMax then
     begin
       CheckRange;
     end;
   end
   else
   begin
     FMax := Value;
     FMin := Value;
     If CheckMax or CheckMin then
     begin
       CheckRange;
     end;
{     Raise EArgusMinMaxError.Create('EArgusMinMaxError: '
       + 'TArgusDataEntry.Max must be greater than or equal to than '
       + 'TArgusDataEntry.Min');}
   end;
  end;
end;

procedure TArgusDataEntry.SetMin(Value : extended);
begin
  if FMin <> Value then
  begin
    if FMax >= Value then
    begin
      FMin := Value;
      If CheckMin then
      begin
        CheckRange;
      end;
    end
    else
    begin
      FMax := Value;
      FMin := Value;
      If CheckMax or CheckMin then
      begin
        CheckRange;
      end;
{           Raise EArgusMinMaxError.Create('EArgusMinMaxError: '
           + 'TArgusDataEntry.Max must be greater than or equal to than '
           + 'TArgusDataEntry.Min');}
    end;
  end;
end;

procedure TArgusDataEntry.SetRealValue(const Value: Double);
begin
  Assert(DataType = dtReal);
  Text := FloatToStr(Value);
end;

procedure TArgusDataEntry.SetCheckMax(Value : boolean);
begin
  if (FCheckMax <> Value) then FCheckMax := Value;
  if FCheckMax then CheckRange;
end;

procedure TArgusDataEntry.SetCheckMin(Value : boolean);
begin
  if (FCheckMin <> Value) then FCheckMin := Value;
  if FCheckMin then CheckRange;
end;



procedure TArgusDataEntry.ExceedingBounds;
begin
  if Assigned(FOnExceedingBounds) then FOnExceedingBounds(Self);
end;

procedure TArgusDataEntry.ExceededBounds;
begin
  if Assigned(FOnExceededBounds) then FOnExceededBounds(Self);
end;

procedure TArgusDataEntry.DataTypeChanged;
begin
  if Assigned(FOnDataTypeChanged) then FOnDataTypeChanged(Self);
end;

procedure TArgusDataEntry.StyleChanged;
begin
  if Assigned(FOnStyleChanged) then FOnStyleChanged(Self);
end;


procedure TArgusDataEntry.Change;
begin
   TextChanged;
   inherited;
end;

procedure TArgusDataEntry.DoExit;
begin
  if (Text = '') and ((DataType = dtInteger) or (DataType = dtReal)) then
  begin
    Text := '0'
  end;
  CheckRange;
  if ( (Copy(Text,Length(Text),1) = '-') or (Copy(Text,Length(Text),1) = '+') )
    and ((FDataType = dtInteger) or (FDataType = dtReal))
  then
  begin
    if (self <> nil) and not (csLoading in self.ComponentState) then
      begin
        SetFocus;
        Beep;
      end;
    self.selStart := Length(Text);
  end
  else if (FDataType = dtReal) and ('e' = LowerCase(Copy(Text,Length(Text),1)))
    then
  begin
    if (self <> nil) and not (csLoading in self.ComponentState) then
    begin
      SetFocus;
      Beep;
    end;
    self.selStart := Length(Text);
  end;
  inherited;

end;

constructor TArgusDataEntry.Create(AOwner: TComponent);
    {Create creates and initializes a TArgusDataEntry instance.}
begin
  inherited Create(AOwner);
  FDataType := dtString;
  FOutput := '0';
  Style := csSimple;
  FMax := 1;
  FMin := 0;
  FCheckMax := False;
  FCheckMin := False;
  Height := 22;
  enabled := True;
  FChangeDisabledColor := True;
  FDisabledColor := clBtnFace;
  FEnabledColor := Color;
  {$IFDEF Delphi_2009_UP}
  FDecimalDelimiter := FormatSettings.DecimalSeparator;
  {$ELSE}
  FDecimalDelimiter := DecimalSeparator;
  {$ENDIF}
  FAppEvents := TApplicationEvents.Create(self);
  FAppEvents.OnSettingChange := SettingsChanged;
end;


function TArgusDataEntry.ChangeTextToInteger : string;
      {ChangeTextToInteger takes the string in the Text property and attempts to
      convert it to an integer. If it fails, it strips one character at a time
      from the endt of Text until it does succeed. It converts '' to '0'}
var
  E : integer;
begin
  Result := Text;
  LocalizeString(Result);
  Text := Result;

  repeat
    E:= 0;
    Val(Result, DummyInteger, E);
    if (E <> 0) and (Length(Result) > 0) then
    begin
      Result := Copy(Result,1,E-1);
    end;

    //if the string is '', convert it to '0'.
    if Length(Result) = 0 then
    begin
      Result := '0'
    end;
  Until (E = 0) or (Result = '+') or (Result = '-');
end;

function TArgusDataEntry.ChangeTextToReal: string;
      {ChangeTextToReal takes the string in the Text property and attempts to
      convert it to an extended real number. If it fails, it strips one character at a time
      from the endt of Text until it does succeed. It converts '' to '0'}
var
  ConversionOK : boolean;
  AFloat : extended;
begin
  ConversionOK := false;
  Result := Text;
  LocalizeString(Result);
  Text := Result;
  While not ConversionOK do
    begin
      ConversionOK := TextToFloat(PChar(Result), AFloat, fvExtended);

      if Not ConversionOK and (Length(Result) > 0) then
      begin
        Result := Copy(Result,1,Length(Result)-1);
      end;

      {if the string is '', convert it to '0'.}
      if Length(Result) = 0 then
      begin
        Result := '0'
      end;

      if ConversionOK and (AFloat > MaxDouble) then
      begin
        ConversionOK := false;
        Result := Copy(Result,1,Length(Result)-1);
      end;
    end;
end;

function TArgusDataEntry.ChangeTextToBoolean : string;
      {ChangeTextToBoolean converts the string in the Text property to '0' if
       the Text is '', '0' or 'false'. The check that the Text is equal to
      'false' is case insensitive.}
begin
  if (Text = '') or (Text = '0') or (Lowercase(Text) = 'false') then
  begin
    Result := '0';
  end
  else
  begin
    Result := '1';
  end;
end;

procedure TArgusDataEntry.SetDataType(ADataType : TDataType);
      {SetDataType sets the DataType property. It also changes the Style of the
       TArgusDataEntry depending on the Data type. dtString, dtInteger, and
       dtReal result in the style being set to csSimple. dtBoolean
       results in the Style being set to csDropDownList. If the data type is set
       to dtCustom, an Style is allowed. It also changes the text to make sure
       it is consistent with the data type and updates Output to be consistent
       with Text. Items[0] and Items[1] are reset to 'False' and 'True'
       respectively every time the data type is set to dtBoolean.}
begin
  if ADataType <> FDataType then
  begin
    FDataType := ADataType;
    case FDataType of
      dtString :
        begin
          Style := csSimple;
          FOutput := Text;
        end;
      dtInteger :
        begin
          Style := csSimple;
          FOutput := ChangeTextToInteger;
          If Text <> FOutput then
          begin
            Beep;
            self.selStart := Length(Text);
          end;
          ItemIndex := -1;
          Text := FOutput;
          Change;
          CheckRange;
        end;
      dtReal :
        begin
          Style := csSimple;
          FOutput := ChangeTextToReal;
          If Text <> FOutput then
          begin
            Beep;
            self.selStart := Length(Text);
          end;
          ItemIndex := -1;
          Text := FOutput;
          Change;
          CheckRange;
        end;
      dtBoolean :
        begin
          Style := csDropDownList;
          FOutput := ChangeTextToBoolean;
          Items.Clear;
          if Items.Count = 0 then
          begin
            Items.Add('False');
            Items.Add('True');
          end ;

          if FOutput = '0' then
          begin
            ItemIndex := 0;
          end
          else
          begin
            ItemIndex := 1;
          end;
          Change;
        end;
      dtCustom :
        begin
          FOutput := Text;
        end;
    end;
    Update;
    DataTypeChanged;
  end;
end;

procedure TArgusDataEntry.SetStyle(Value: TComboBoxStyle);
      {SetStyle changes to Style of the TArgusDataEntry and checks that the style is
       consistent with the DataType.}
begin
  inherited;
  if Value <> Style then
    begin
      Case Value of
        csDropDown:
          begin
            FStyle :=csDropDown;
            DataType := dtCustom;
          end;
        csOwnerDrawFixed:
          begin
            FStyle :=csOwnerDrawFixed;
            DataType := dtCustom;
          end;
        csOwnerDrawVariable :
          begin
            FStyle :=csOwnerDrawVariable;
            DataType := dtCustom;
          end;
        csSimple :
          begin
            FStyle :=csSimple;
            if FDataType = dtBoolean then
            begin
              DataType := dtCustom
            end;
          end;
        csDropDownList :
          begin
            FStyle :=csDropDownList;
            if (FDataType <> dtBoolean) and (FDataType <> dtCustom) then
            begin
              DataType := dtCustom
            end;
          end;
      end;
      StyleChanged;
    end;
  Update;
  StyleChanged;
end;

procedure TArgusDataEntry.TextChanged;
      {TextChanged is called before the event handler for the OnChanged event.
      It checks that the data entered in the TArgusDataEntry is valid. It does
      not reject '-' even though that is not a valid integer or real number.
      However, if the text is '-' when OnExit occurs, the CheckRange procedure
      will catch the error.}
begin
  case FDataType of
    dtString :
      begin
        FOutput := Text;
      end;
    dtInteger :
      begin
        FOutput := ChangeTextToInteger;
        if (text <> FOutput) and (text <> '-') and (Text <> '') then
        begin
          Text := FOutput;
          Beep;
          selStart := Length(Text);
        end;
      end;
    dtReal :
      begin
        FOutput := ChangeTextToReal;
        if (text <> FOutput) and (text <> '-') and (Text <> '') then
        begin
          Text := FOutput;
          Beep;
          selStart := Length(Text);
        end;
      end;
    dtBoolean :
      begin
        FOutput := ChangeTextToBoolean;
          if (text <> FOutput) and (Lowercase(Text) <> 'true')
            and (Lowercase(Text) <> 'false') then
          begin
            if FOutput = '0' then
            begin
              ItemIndex := 0;
            end
            else
            begin
              ItemIndex := 1;
            end;
            Beep;
            self.selStart := Length(Text);
          end
          else
          begin
            if FOutput = '0' then
            begin
              ItemIndex := 0;
            end
            else
            begin
              ItemIndex := 1;
            end;
          end;
      end;
    dtCustom :
      begin
        FOutput := Text;
      end;
  end;
  Update;
end;

function TArgusDataEntry.TryGetRealValue(var AValue: Double): Boolean;
begin
  Result := TextToFloat(Text, AValue);
end;

procedure TArgusDataEntry.CheckRange;
      {CheckRange is called before the event handler for the OnExit event.
      It checks that the data in the TArgusDataEntry do not exceed the
      specified limits. However, it only checks the range if the DataType is
      dtInteger or dtReal. It also catches the invalid value of '-' for real
      and integer data typed in the text field.}
var
  BeyondBounds : boolean;
  NeedToChange: Boolean;
begin
  if ( (Copy(Text,Length(Text),1) = '-') or (Copy(Text,Length(Text),1) = '+') )
    and ((FDataType = dtInteger) or (FDataType = dtReal)) then
  begin
    if (self <> nil) and not (csLoading in self.ComponentState) then SetFocus;
    selStart := Length(Text);
  end
  else if (FDataType = dtReal) and ('e' = LowerCase(Copy(Text,Length(Text),1)))
    then
  begin
    if (self <> nil) and not (csLoading in self.ComponentState) then SetFocus;
    selStart := Length(Text);
  end
  else
  begin
    BeyondBounds := False;
    if CheckMax and ((FDataType = dtInteger) or (FDataType = dtReal)) then
    begin
      if (LocalStrToFloat(Text) > FMax) then
      begin
        if FDataType = dtInteger then
        begin
          NeedToChange := (Text <> IntToStr(Round(FMax)));
        end
        else
        begin
          NeedToChange := (Text <> FloatToStr(FMax));
        end;
        if NeedToChange then
        begin
          ExceedingBounds;
          BeyondBounds := True;
          if CheckMax and ((FDataType = dtInteger) or (FDataType = dtReal)) then
          begin
            if LocalStrToFloat(Text) > FMax then
            begin
              if (FDataType = dtInteger) then
              begin
                Text := IntToStr(Round(FMax));
                Change;
              end
              else
              begin
                Text := FloatToStr(FMax);
                Change;
              end;
              FOutput := Text;
              Beep;
              self.selStart := Length(Text);
              ExceededBounds;
            end;
          end;
        end;
      end;
    end;
    if CheckMin and ((FDataType = dtInteger) or (FDataType = dtReal)) then
    begin
      if (LocalStrToFloat(Text) < FMin) then
      begin
        if FDataType = dtInteger then
        begin
          NeedToChange := (Text <> IntToStr(Round(FMin)));
        end
        else
        begin
          NeedToChange := (Text <> FloatToStr(FMin));
        end;
        if NeedToChange then
        begin
          ExceedingBounds;
          BeyondBounds := True;
          if CheckMin and (FDataType = dtInteger) or (FDataType = dtReal) then
          begin
            if LocalStrToFloat(Text) < FMin then
            begin
              if (FDataType = dtInteger) then
              begin
                Text := IntToStr(Round(FMin));
                Change;
              end
              else
              begin
                Text := FloatToStr(FMin);
                Change;
              end;
              FOutput := Text;
              Beep;
              self.selStart := Length(Text);
              ExceededBounds;
            end;
          end;
        end;
      end;
    end;
    if BeyondBounds then CheckRange;
  end;
end;

procedure TArgusDataEntry.SetText(const Value: TCaption);
var
  NewValue : string;
  CommaSeparator : array[0..255] of Char;
  DecimalLocation : integer;
begin
  NewValue := Value;
  if FDataType = dtReal then
  begin
    GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SDECIMAL,@CommaSeparator,255);
    if String(CommaSeparator) = '.' then
    begin
      DecimalLocation := Pos(',',NewValue);
      if DecimalLocation > 0 then
      begin
        NewValue[DecimalLocation] := '.';
      end;
    end;
    if String(CommaSeparator) = ',' then
    begin
      DecimalLocation := Pos('.',NewValue);
      if DecimalLocation > 0 then
      begin
        NewValue[DecimalLocation] := ',';
      end;
    end;
  end;
  if GetText <> NewValue then
  begin
    SetTextBuf(PChar(NewValue));
    Change;
  end;
end;

procedure TArgusDataEntry.SettingsChanged(Sender: TObject; Flag: Integer;
  const Section: string; var Result: Integer);
var
  DecSep: Char;
begin
  {$IFDEF Delphi_2009_UP}
  DecSep := FormatSettings.DecimalSeparator;
  {$ELSE}
  DecSep := DecimalSeparator;
  {$ENDIF}
  if (DecSep <> FDecimalDelimiter) then
  begin
    if (DataType = dtReal) then
    begin
      Text := StringReplace(Text, FDecimalDelimiter, DecSep, [rfReplaceAll]);
    end;
    FDecimalDelimiter := DecSep;
  end;
end;

function TArgusDataEntry.GetIntegerValue: Integer;
begin
  Assert(DataType = dtInteger);
  result := StrToInt(Text)
end;

function TArgusDataEntry.GetRealValue: Double;
begin
  Assert(DataType = dtReal);
  // If this is changed, change RealValueDefault too.
  if (Text <> '') and (Text <> '+') and (Text <> '-') then
  begin
    result := StrToFloat(Text)
  end
  else
  begin
    Result := 0.0;
  end;
end;

function TArgusDataEntry.GetText: TCaption;
var
  Len: Integer;
begin
  Len := GetTextLen;
  SetString(Result, PChar(nil), Len);
  if Len <> 0 then GetTextBuf(Pointer(Result), Len + 1);
end;

procedure SetMaxDouble;
var
  Index : integer;
begin
  MaxDouble := 1;
  For Index := 1 to 32 do
  begin
    MaxDouble := MaxDouble*2 + 1;
  end;
  For Index := 1 to 991 do
  begin
    MaxDouble := MaxDouble*2 ;
  end;
end;


procedure TArgusDataEntry.SetChangeDisabledColor(const Value: boolean);
begin
  FChangeDisabledColor := Value;
end;

procedure TArgusDataEntry.SetEnabled(Value: Boolean);
begin
  inherited;
  ChangeColor;
end;



procedure TArgusDataEntry.SetDisabledColor(const Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    ChangeColor
  end;
end;

procedure TArgusDataEntry.SetEnabledColor(Value: TColor);
begin
  if FEnabledColor <> Value then
  begin
    FEnabledColor := Value;
    ChangeColor
  end;
end;

procedure TArgusDataEntry.SetIntegerValue(const Value: Integer);
begin
  Assert(DataType = dtInteger);
  Text := IntToStr(Value);
end;

procedure TArgusDataEntry.ChangeColor;
begin
  if csDestroying in ComponentState then
  begin
    Exit;
  end;
  if FChangeDisabledColor then
  begin
   if Enabled then
   begin
     Color := FEnabledColor;
   end
   else
   begin
     Color := FDisabledColor;
   end;
  end;
end;

initialization
begin
  SetMaxDouble;
end;


end.
