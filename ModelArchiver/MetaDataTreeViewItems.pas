// http://sedac.ciesin.columbia.edu/metadata/guide/guide_toc.html
// https://www.fgdc.gov/metadata/csdgm
unit MetaDataTreeViewItems;

interface

uses
  FMX.TreeView, System.Classes, FMX.Types,
  System.SysUtils, System.Types, System.UITypes, System.Variants,
  FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, MetaDataInterfacesUnit,
  Xml.VerySimple, frameCustomMetaDataUnit, System.Generics.Collections;

type
  TCheckType = (ctCheckBox, ctRadioButton);

  TCustomMetaDataItem = class;

  TCreateMethod = function (Parent: TCustomMetaDataItem): TCustomMetaDataItem of Object;

  TRequiredType = (rtOptional, rtManditoryIfApplicable, rtManditory);

  TCustomMetaDataItem = class(TComponent, ICustomMetaData)
  private
    FItem: TTreeViewItem;
    FID: string;
    FRequired: boolean;
    FUsed: Boolean;
    FShortName: string;
    FDescription: string;
    FFrame: TframeCustomMetaData;
    FMoreThanOneAllowed: boolean;
    FLongName: string;
    FOnUsedChanged: TNotifyEvent;
    FRadioGroupName: string;
    FCheckType: TCheckType;
    Fcb: TCheckBox;
    Frb: TRadioButton;
    FCreateMethod: TCreateMethod;
    FHeight: Single;
    XmlAssigned: Boolean;
    FRadioIndex: integer;
    FXmlNode: TXmlNode;
    FOnChange: TNotifyEvent;
    FRequiredType: TRequiredType;
    procedure SetDescription(const Value: string);
    procedure SetID(const Value: string);
    procedure SetRequired(const Value: boolean);
    procedure SetShortName(const Value: string);
    procedure SetUsed(const Value: Boolean);
    function GetUsed: Boolean;
    function GetDescription: string;
    function GetShortName: string;
    function GetRequired: boolean;
    function GetID: string;
    procedure SetMoreThanOneAllowed(const Value: boolean);
    function GetLongName: string;
    procedure SetLongName(const Value: string);
    procedure DoUsedChanged;
    procedure SetCheckType(const Value: TCheckType);
    procedure SetRadioGroupName(const Value: string);
    function GetTextHeight: single;
    procedure SetTextHeight(const Value: single);
    procedure SetRadioIndex(const Value: integer);
    procedure SetXmlNode(const Value: TXmlNode);
    procedure SetRequiredType(const Value: TRequiredType);
  protected
    function GetChildrenUsed: Boolean; virtual;
    function FrameClass: TMetaDataFrameClass; virtual; abstract;
    procedure DoChange;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    procedure ShowFrame;
    procedure HideFrame;
    function GetStringContent: string; virtual;
    property TreeViewItem: TTreeViewItem read FItem;
    procedure WriteChildren(Node: TXmlNode); virtual;
    function NewItem(ParentItem: TCustomMetaDataItem): TCustomMetaDataItem; Virtual;
    procedure ApplyCheckType;
    function FullRadioGroupName: string;
    procedure RadioButtonChecked(Sender: TObject);
    procedure CheckBoxChecked(Sender: TObject);
    property CheckBox: TCheckBox read Fcb;
    property RadioButton: TRadioButton read Frb;
    property CreateMethod: TCreateMethod read FCreateMethod write FCreateMethod;
    function FullUsed: boolean;
    function ParentItem: TCustomMetaDataItem;
    procedure AssignValuesFromXML(Node: TXmlNode); virtual;
    property RadioIndex: integer read FRadioIndex write SetRadioIndex;
    property XmlNode: TXmlNode read FXmlNode write SetXmlNode;
  published
    // @name describes what the item represents
    property Description: string read GetDescription write SetDescription;
    // @name is the term used for the item in the XML file.
    property LongName: string read GetLongName write SetLongName;
    // @name is the term used for the item in the XML file.
    property ShortName: string read GetShortName write SetShortName;
    // @name records whether or not an item should be included in the metadata.
    // If any children of an item should be included, it must be included too.
    property Used: Boolean read GetUsed write SetUsed;
    // If @name is true, the item must be included in the metadata.
    property Required: boolean read GetRequired write SetRequired;
    property RequiredType: TRequiredType read FRequiredType write SetRequiredType;
    // @name is the numeric text used to refer to the item in
    // https://www.fgdc.gov/metadata/csdgm. For example: "1.2.3".
    property ID: string read GetID write SetID;
    property MoreThanOneAllowed: boolean read FMoreThanOneAllowed
      write SetMoreThanOneAllowed default True;
    property OnUsedChanged: TNotifyEvent read FOnUsedChanged write FOnUsedChanged;
    property CheckType: TCheckType read FCheckType write SetCheckType;
    property RadioGroupName: string read FRadioGroupName write SetRadioGroupName;
    property TextHeight: single read GetTextHeight write SetTextHeight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCompoundItemMetaDataItem = class(TCustomMetaDataItem)
  private
    function GetNodeFromText(Node: TXmlNode): TCustomMetaDataItem;
  protected
    function GetChildrenUsed: Boolean; override;
    function FrameClass: TMetaDataFrameClass; override;
  public
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  end;

  TNumericMetaDataItem = class(TCustomMetaDataItem, INumericInterface)
  private
    FContent: double;
    FMin: double;
    FMax: double;
    FMinLimitType: TMinLimitType;
    FMaxLimitType: TMaxLimitType;
    procedure SetContent(const Value: double);
    function GetContent: double;
    function GetMaxValue: double;
    function GetMinValue: double;
    procedure SetMaxValue(const Value: double);
    procedure SetMinValue(const Value: double);
    function GetMaxLimitType: TMaxLimitType;
    function GetMinLimitType: TMinLimitType;
    procedure SetMaxLimitType(const Value: TMaxLimitType);
    procedure SetMinLimitType(const Value: TMinLimitType);
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    function GetStringContent: string; override;
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  published
    // @name is the numeric value of the item.
    property Content: double read GetContent write SetContent;
    property MinValue: double read GetMinValue write SetMinValue;
    property MaxValue: double read GetMaxValue write SetMaxValue;
    property MinLimitType: TMinLimitType read GetMinLimitType write SetMinLimitType;
    property MaxLimitType: TMaxLimitType read GetMaxLimitType write SetMaxLimitType;
  end;

  TIntegerMetaDataItem = class(TCustomMetaDataItem, IIntegerInterface)
  private
    FContent: integer;
    FMax: Integer;
    FMin: Integer;
    FIllegalValues: TList<Integer>;
    procedure SetContent(const Value: integer);
    function GetContent: integer;
    function GetMaxValue: Integer;
    function GetMinValue: Integer;
    procedure SetMaxValue(const Value: Integer);
    procedure SetMinValue(const Value: Integer);
    function GetIllegalValue(Index: Integer): Integer;
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetStringContent: string; override;
    property IllegalValues[Index: Integer]: Integer read GetIllegalValue;
    procedure AddIllegalValue(Value: Integer);
    function IllegalValueCount: Integer;
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  published
    // @name is the numeric value of the item.
    property Content: integer read GetContent write SetContent;
    property MinValue: Integer read GetMinValue write SetMinValue default -Maxint;
    property MaxValue: Integer read GetMaxValue write SetMaxValue default -Maxint;
  end;

  TTextMetaDataItem = class(TCustomMetaDataItem, ITextMetaDataItem)
  private
    FContent: String;
    procedure SetContent(const Value: String);
    function GetContent: String;
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetStringContent: string; override;
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  published
    // @name is the text of the item.
    property Content: String read GetContent write SetContent;
  end;

  TChoiceMetaDataItem = class(TCustomMetaDataItem, IChoiceMetaDataItem)
  private
    FContent: string;
    FChoices: TStrings;
    FAltChoices: TStrings;
    FOnUpdateChoices: TNotifyEvent;
    function GetContent: string;
    procedure SetContent(const Value: string);
    procedure SetChoices(const Value: TStrings);
    function GetChoices: TStrings;
    function GetOnUpdateChoices: TNotifyEvent;
    procedure SetOnUpdateChoices(const Value: TNotifyEvent);
    function GetObject: TObject;
    function GetAltChoices: TStrings;
    procedure SetAltChoices(const Value: TStrings);
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetStringContent: string; override;
    property OnUpdateChoices: TNotifyEvent read GetOnUpdateChoices
      write SetOnUpdateChoices;
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  published
    // @name is the text of the item.
    property Choice: string read GetContent write SetContent;
    property Choices: TStrings read GetChoices write SetChoices;
    property AlternativeChoices: TStrings read GetAltChoices write SetAltChoices;
  end;

  TChoicePlusMetaDataItem = class(TChoiceMetaDataItem, IChoicePlusMetaDataItem)
  private
    FFreeContent: string;
    function GetFreeContent: string;
    procedure SetFreeContent(const Value: string);
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetStringContent: string; override;
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  published
    property FreeContent: string read GetFreeContent write SetFreeContent;
  end;

  TChoicePlusDateMetaDataItem = class(TChoiceMetaDataItem,
    IChoicePlusDateMetaDataItem, IDate)
  private
//    FDate: TDate;
    FDateFormat: TDateFormat;
    FYear: Integer;
    FMonth: Integer;
    FDay: Integer;
    function GetDateFormat: TDateFormat;
    procedure SetDateFormat(const Value: TDateFormat);
    function GetDateIntf: IDate;
//    procedure SetContent(const Value: TDate);
//    function GetContent: TDate;
    function GetYear: Integer;
    procedure SetYear(const Value: Integer);
    function GetDay: integer;
    function GetMonth: integer;
    procedure SetDay(const Value: integer);
    procedure SetMonth(const Value: integer);
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetStringContent: string; override;
    property DateIntf: IDate read GetDateIntf;
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  published
    property DateFormat: TDateFormat read GetDateFormat write SetDateFormat;
//    property Content: TDate read GetContent write SetContent;
    property Year: Integer read GetYear write SetYear;
    property Month: integer read GetMonth write SetMonth;
    property Day: integer read GetDay write SetDay;
  end;

  TDateMetaDataItem = class(TCustomMetaDataItem, IDateMetaDataItem, IDate)
  private
//    FDate: TDate;
    FDateFormat: TDateFormat;
    FYear: Integer;
    FMonth: Integer;
    FDay: Integer;
//    procedure SetContent(const Value: TDate);
//    function GetContent: TDate;
    function GetDateFormat: TDateFormat;
    procedure SetDateFormat(const Value: TDateFormat);
    function GetYear: Integer;
    procedure SetYear(const Value: Integer);
    function GetDateIntf: IDate;
    function GetDay: integer;
    function GetMonth: integer;
    procedure SetDay(const Value: integer);
    procedure SetMonth(const Value: integer);
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    function GetStringContent: string; override;
    property DateIntf: IDate read GetDateIntf;
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  published
    // @name is the text of the item.
//    property Content: TDate read GetContent write SetContent;
    property DateFormat: TDateFormat read GetDateFormat write SetDateFormat;
    property Year: Integer read GetYear write SetYear;
    property Month: integer read GetMonth write SetMonth;
    property Day: integer read GetDay write SetDay;
  end;

  TExtentDataItem = class(TCustomMetaDataItem, IExtentDataItem)
  private
    FNorth: double;
    FSouth: double;
    FEast: double;
    FWest: double;
    function GetEast: double;
    function GetNorth: double;
    function GetSouth: double;
    function GetWest: double;
    procedure SetEast(const Value: double);
    procedure SetNorth(const Value: double);
    procedure SetSouth(const Value: double);
    procedure SetWest(const Value: double);
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure WriteChildren(Node: TXmlNode); override;
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  published
    property North: double read GetNorth write SetNorth;
    property South: double read GetSouth write SetSouth;
    property East: double read GetEast write SetEast;
    property West: double read GetWest write SetWest;
  end;

  TPolygonMetaDataItem = class(TCustomMetaDataItem, IPolygonDataItem)
  private
    FPolygon: TPolygon;
    procedure SetPolygon(const Value: TPolygon);
    function GetPolygon: TPolygon;
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteChildren(Node: TXmlNode); override;
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  published
    property Polygon: TPolygon read GetPolygon write SetPolygon;
  end;

  TTimeMetaDataItem = class(TCustomMetaDataItem, ITimeDataItem, ITime)
  private
    FTime: TTime;
    FFormatChoice: TTimeChoice;
    FDifferentialHours: integer;
    FDifferentialMinutes: Integer;
    function GetDifferentialHours: integer;
    function GetDifferentialMinutes: Integer;
    function GetFormatChoice: TTimeChoice;
    function GetTime: TTime;
    procedure SetDifferentialHours(const Value: integer);
    procedure SetDifferentialMinutes(const Value: Integer);
    procedure SetFormatChoice(const Value: TTimeChoice);
    procedure SetTime(const Value: TTime);
    function GetTimeIntf: ITime;
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    function GetStringContent: string; override;
    procedure AssignValuesFromXML(Node: TXmlNode); override;
  published
    property Time: TTime read GetTime write SetTime;
    property FormatChoice: TTimeChoice read GetFormatChoice write SetFormatChoice;
    property DifferentialHours: integer read GetDifferentialHours write SetDifferentialHours;
    property DifferentialMinutes: Integer read GetDifferentialMinutes write SetDifferentialMinutes;
    property TimeIntf: ITime read GetTimeIntf;
  end;

  TChoicePlusTimeDataItem = class(TChoiceMetaDataItem, IChoicePlusTimeMetaDataItem, ITime)
  private
    FTime: TTime;
    FFormatChoice: TTimeChoice;
    FDifferentialHours: integer;
    FDifferentialMinutes: Integer;
    function GetTimeIntf: ITime;
    function GetDifferentialHours: integer;
    function GetDifferentialMinutes: Integer;
    function GetFormatChoice: TTimeChoice;
    function GetTime: TTime;
    procedure SetDifferentialHours(const Value: integer);
    procedure SetDifferentialMinutes(const Value: Integer);
    procedure SetFormatChoice(const Value: TTimeChoice);
    procedure SetTime(const Value: TTime);
  protected
    function FrameClass: TMetaDataFrameClass; override;
  public
    procedure AssignValuesFromXML(Node: TXmlNode); override;
    function GetStringContent: string; override;
  published
    property Time: TTime read GetTime write SetTime;
    property FormatChoice: TTimeChoice read GetFormatChoice write SetFormatChoice;
    property DifferentialHours: integer read GetDifferentialHours write SetDifferentialHours;
    property DifferentialMinutes: Integer read GetDifferentialMinutes write SetDifferentialMinutes;
    property TimeIntf: ITime read GetTimeIntf;
  end;

implementation

uses
  frameNumericUnit, frmMetaDataUnit, frameTextUnit, frameDateUnit,
  frameChoiceUnit, frameExtentUnit, frameChoicePlusUnit,
  frameChoicePlusDateUnit, System.Math, framePolygonUnit,
  frameIntegerMetaDataUnit, frameTimeUnit, frameChoicePlusTimeUnit,
  System.StrUtils, frameMetaDataEditorUnit;

var
  RealSettings: TFormatSettings;


function DateStringContent(DateIntf: IDate): string;
var
  ResultBuilder: TStringBuilder;
begin
  ResultBuilder := TStringBuilder.Create;
  try
    if DateIntf.Year <= -10000 then
    begin
      ResultBuilder.Append('cc');
      DateIntf.Year := -DateIntf.Year;
      DateIntf.DateFormat := dfYear;
    end
    else if DateIntf.Year <= 0 then
    begin
      ResultBuilder.Append('bc');
      DateIntf.Year := -DateIntf.Year;
    end;
    ResultBuilder.Append(Format('%.4d', [DateIntf.Year]));
    if DateIntf.DateFormat in [dfYearMonth, dfYearMonthDay] then
    begin
      ResultBuilder.Append(Format('%.2d', [DateIntf.Month]));
      if DateIntf.DateFormat = dfYearMonthDay then
      begin
        ResultBuilder.Append(Format('%.2d', [DateIntf.Day]));
      end;
    end;
    result := ResultBuilder.ToString;
  finally
    ResultBuilder.Free;
  end;

end;

function ConvertDate(Item: IDate; DString: string): boolean;
var
  Prefix: string;
  DateString: string;
  AValue: Integer;
  function ConvertDateString: boolean;
  var
    YearString: string;
    MonthString: string;
    DayString: string;
  begin
    YearString := Copy(DateString,1,4);
    MonthString := Copy(DateString, 5, 2);
    DayString := Copy(DateString, 7, 2);
    result := TryStrToInt(YearString, AValue);
    if result then
    begin
      Item.Year := AValue;
      Item.DateFormat := dfYear;
      if MonthString <> '' then
      begin
        result := TryStrToInt(MonthString, AValue);
        if result then
        begin
          Item.Month := AValue;
          Item.DateFormat := dfYearMonth;
          if DayString <> '' then
          begin
            result := TryStrToInt(DayString, AValue);
            if result then
            begin
              Item.Day := AValue;
              Item.DateFormat := dfYearMonthDay;
            end;
          end;
        end;
      end;
    end;
  end;
begin
  result := False;
  if DString <> '' then
  begin
    Prefix := LowerCase(Copy(DString, 1, 2));
    if Prefix = 'cc' then
    begin
      DateString := Copy(DString, 3, MAXINT);
      Item.DateFormat := dfYear;
      result := TryStrToInt(DateString, AValue);
      if result then
      begin
        Item.Year := -AValue;
      end;
    end
    else if Prefix = 'bc' then
    begin
      DateString := Copy(DString, 3, MAXINT);
      result := ConvertDateString;
      Item.Year := -Item.Year;
    end
    else
    begin
      DateString := DString;
      result := ConvertDateString;
    end;
  end;
end;

function TryFortranStrToFloat(AString: string; var Value: Extended): Boolean;
var
  OldDecimalSeparator: Char;
  SignPos: Integer;
begin
  AString := Trim(AString);
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    AString := StringReplace(AString, ',', '.', [rfReplaceAll, rfIgnoreCase]);
    AString := StringReplace(AString, 'd', 'e', [rfReplaceAll, rfIgnoreCase]);
    SignPos := Max(PosEx('+', AString, 2), PosEx('-', AString, 2));
    if SignPos > 0 then
    begin
      if not CharInSet(AString[SignPos-1], ['e', 'E']) then
      begin
        Insert('E', AString, SignPos);
      end;
    end;
    result := TryStrToFloat(AString, Value);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function FortranStrToFloatDefault(AString: string; DefaultValue: Extended): Extended;
var
  OldDecimalSeparator: Char;
  SignPos: Integer;
begin
  AString := Trim(AString);
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    AString := StringReplace(AString, ',', '.', [rfReplaceAll, rfIgnoreCase]);
    AString := StringReplace(AString, 'd', 'e', [rfReplaceAll, rfIgnoreCase]);
    SignPos := Max(PosEx('+', AString, 2), PosEx('-', AString, 2));
    if SignPos > 0 then
    begin
      if not CharInSet(AString[SignPos-1], ['e', 'E']) then
      begin
        Insert('E', AString, SignPos);
      end;
    end;
    result := StrToFloatDef(AString, DefaultValue);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function ConvertTimeStr(Item: ITime; TimeString: string): boolean;
var
  SignPos: Integer;
  DifferentialSign: Integer;
  ZPos: Integer;
  TString: string;
  Differential: String;
  HourDiff: string;
  AValue: Integer;
  MinuteDiff: string;
  HourString: string;
  MinuteString: string;
  SecondString: string;
  Hours: Integer;
  Minutes: Integer;
  Seconds: Integer;
begin
  if TimeString = '' then
  begin
    result := False;
    Exit;
  end;
  DifferentialSign := 0;
  SignPos := Pos('+', TimeString);
  if SignPos > 0 then
  begin
    Item.FormatChoice := tcLocalDifferential;
    DifferentialSign := 1;
  end
  else
  begin
    SignPos := Pos('-', TimeString);
    if SignPos > 0 then
    begin
      Item.FormatChoice := tcLocalDifferential;
      DifferentialSign := -1;
    end
    else
    begin
      ZPos := Pos('Z', TimeString);
      if ZPos > 0 then
      begin
        Item.FormatChoice := tcUniversal;
      end
      else
      begin
        Item.FormatChoice := tcLocal;
      end;
    end;
  end;
  result := True;
  case Item.FormatChoice of
    tcLocal: TString := TimeString;
    tcLocalDifferential:
      begin
        TString := Copy(TimeString, 1, SignPos-1);
        Differential := Copy(TimeString, SignPos+1, 4);
        HourDiff := Copy(Differential, 1, 2);
        result := TryStrToInt(HourDiff, AValue);
        if result then
        begin
          Item.DifferentialHours := DifferentialSign*AValue;
          MinuteDiff := Copy(Differential, 3, 2);
          result := TryStrToInt(MinuteDiff, AValue);
          if result then
          begin
            Item.DifferentialMinutes := AValue;
          end;
        end;
      end;
    tcUniversal:
      begin
        TString := Copy(TimeString, 1, SignPos-1);
      end;
  end;
  if result then
  begin
    HourString := Copy(TString, 1,2);
    MinuteString := Copy(TString, 3,2);
    SecondString := Copy(TString, 5,MaxInt);
    result := TryStrToInt(HourString, Hours)
      and TryStrToInt(MinuteString, Minutes)
      and TryStrToInt(SecondString, Seconds);
    if result then
    begin
      Item.Time := Seconds/100;
      Item.Time := (Minutes + Item.Time)/60;
      Item.Time := (Hours + Item.Time)/24;
    end;
  end;
end;

function TimeToMetaStr(Time: ITime): string;
var
  SignSym: string;
begin
  case Time.FormatChoice of
    tcLocal:
      begin
        Result := FormatDateTime('hhnnsszzz', Time.Time);
      end;
    tcLocalDifferential:
      begin
        if Time.DifferentialHours >= 0 then
        begin
          SignSym := '+';
        end
        else
        begin
          SignSym := '-';
        end;
        Result := FormatDateTime('hhnnsszzz', Time.Time) + SignSym
          + Format('%0:.2d%1:.2d', [Time.DifferentialHours, Time.DifferentialMinutes]);
      end;
    tcUniversal:
      begin
        Result := FormatDateTime('hhnnsszzz', Time.Time) + 'Z';
      end;
  end;
end;

{ TCustomMetaDataItem }

procedure TCustomMetaDataItem.ApplyCheckType;
begin
  FreeAndNil(Frb);
  FreeAndNil(Fcb);
  case CheckType of
    ctCheckBox:
      begin
        Fcb := TCheckBox.Create(self);
        Fcb.Parent := TreeViewItem;
        Fcb.Align := TAlignLayout.Left;
        Fcb.Text := '';
        Fcb.IsChecked := TreeViewItem.IsChecked;
        Fcb.OnChange := CheckBoxChecked;
        if MoreThanOneAllowed then
        begin
          Fcb.Margins.Left := 40;
        end
        else
        begin
          Fcb.Margins.Left := 20;
        end;
        Fcb.Width := 20;
//        Fcb.Visible := False;
      end;
    ctRadioButton:
      begin
        Frb := TRadioButton.Create(self);
        Frb.Parent := TreeViewItem;
        Frb.Align := TAlignLayout.Left;
        Frb.Text := '';
        Frb.IsChecked := TreeViewItem.IsChecked;
        Frb.OnChange := RadioButtonChecked;
        Frb.GroupName := Self.FullRadioGroupName;
        if MoreThanOneAllowed then
        begin
          Frb.Margins.Left := 40;
        end
        else
        begin
          Frb.Margins.Left := 20;
        end;
        Frb.Width := 20;
//        Frb.Visible := False;
      end;
  end;
end;

procedure TCustomMetaDataItem.Assign(Source: TPersistent);
var
  SourceItem: TCustomMetaDataItem;
begin
  if Source is TCustomMetaDataItem then
  begin
    SourceItem := TCustomMetaDataItem(Source);
    Description := SourceItem.Description;
    ShortName := SourceItem.ShortName;
    LongName := SourceItem.LongName;
    Used := SourceItem.Used;
    Required := SourceItem.Required;
    ID := SourceItem.ID;
    MoreThanOneAllowed := MoreThanOneAllowed;
    OnUsedChanged := SourceItem.OnUsedChanged;
  end
  else
  begin
    inherited;
  end;
end;

procedure TCustomMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
begin
  XmlAssigned := True;
  Used := True;
  XmlNode := Node;
end;

procedure TCustomMetaDataItem.CheckBoxChecked(Sender: TObject);
var
  Item: TTreeViewItem;
  cb: TCheckBox;
  MetaData: TCustomMetaDataItem;
begin
  cb := Sender as TCheckBox;
  Item := cb.Parent as TTreeViewItem;
  MetaData := Item.TagObject as TCustomMetaDataItem;
  Item.IsChecked := cb.IsChecked or MetaData.Required;
//  cb.IsChecked := Item.IsChecked;
//
//  rb := Sender as TRadioButton;
//  Item := rb.Parent as TTreeViewItem;
//  Item.IsChecked := rb.IsChecked;

end;

constructor TCustomMetaDataItem.Create(AOwner: TComponent);
begin
  inherited;
  FItem := AOwner as TTreeViewItem;
  FMoreThanOneAllowed := False;
  FHeight := 80;
  FXmlNode := nil;
end;

procedure TCustomMetaDataItem.DoChange;
begin
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TCustomMetaDataItem.DoUsedChanged;
begin
  if Assigned(OnUsedChanged) then
  begin
    OnUsedChanged(Self);
  end;
end;

function TCustomMetaDataItem.FullRadioGroupName: string;
var
  GroupNames: TStringBuilder;
  ParentTreeViewItem: TTreeViewItem;
  ParentMetaData: TCustomMetaDataItem;
begin
  GroupNames := TStringBuilder.Create;
  try
    GroupNames.Append(RadioGroupName);
    GroupNames.Append(RadioIndex);
    ParentTreeViewItem := TreeViewItem.ParentItem;
    while ParentTreeViewItem <> nil do
    begin
      ParentMetaData := ParentTreeViewItem.TagObject as TCustomMetaDataItem;
      GroupNames.Append(ParentMetaData.ShortName);
      GroupNames.Append(ParentMetaData.RadioIndex);
      ParentTreeViewItem := ParentTreeViewItem.ParentItem;
    end;
    result := GroupNames.ToString;
  finally
    GroupNames.Free;
  end;
end;

function TCustomMetaDataItem.FullUsed: boolean;
var
  ParentMetaData: TCustomMetaDataItem;
begin
  result := Used;
  if result then
  begin
    ParentMetaData := ParentItem;
    while ParentMetaData <> nil do
    begin
      result := ParentMetaData.Used;
      if not result then
      begin
        Exit;
      end;
      ParentMetaData := ParentMetaData.ParentItem;
    end;
  end;
end;

function TCustomMetaDataItem.GetChildrenUsed: Boolean;
begin
  result := False;
end;

function TCustomMetaDataItem.GetDescription: string;
begin
  result := FDescription;
end;

function TCustomMetaDataItem.GetID: string;
begin
  result := FID;
end;

function TCustomMetaDataItem.GetLongName: string;
begin
  Result := FLongName;
end;

function TCustomMetaDataItem.GetRequired: boolean;
begin
  result := FRequired;
end;

function TCustomMetaDataItem.GetShortName: string;
begin
  result := FShortName;
end;

function TCustomMetaDataItem.GetStringContent: string;
begin
  result := '';
end;

function TCustomMetaDataItem.GetTextHeight: single;
begin
  result := FHeight;
end;

function TCustomMetaDataItem.GetUsed: Boolean;
begin
  Result := FUsed or Required;
end;

procedure TCustomMetaDataItem.HideFrame;
begin
  FreeAndNil(FFrame);
end;

function TCustomMetaDataItem.NewItem(ParentItem: TCustomMetaDataItem): TCustomMetaDataItem;
  procedure AssignRadioGroupName(AnItem: TCustomMetaDataItem);
  var
    ChildIndex: Integer;
    ChildItem: TCustomMetaDataItem;
  begin
    if AnItem.CheckType = ctRadioButton then
    begin
      AnItem.Frb.GroupName := AnItem.FullRadioGroupName;
    end;
    for ChildIndex := 0 to AnItem.TreeViewItem.Count - 1 do
    begin
      ChildItem :=AnItem.TreeViewItem.Items[ChildIndex].TagObject as TCustomMetaDataItem;
      AssignRadioGroupName(ChildItem);
    end;
  end;
var
  LastSimilarSibling: TCustomMetaDataItem;
  ParentTreeViewItem: TTreeViewItem;
  ChildIndex: Integer;
  Sibling: TCustomMetaDataItem;
begin
  LastSimilarSibling := self;
  ParentTreeViewItem := ParentItem.TreeViewItem;
  for ChildIndex := ParentTreeViewItem.Count - 1 downto TreeViewItem.Index+1 do
  begin
    Sibling := ParentTreeViewItem.Items[ChildIndex].TagObject as TCustomMetaDataItem;
    if Sibling.ShortName = ShortName then
    begin
      LastSimilarSibling := Sibling;
      break;
    end;
  end;
  result := CreateMethod(ParentItem);
  result.TreeViewItem.Index := TreeViewItem.Index+1;
  result.CheckType := ctCheckBox;
  result.MoreThanOneAllowed := MoreThanOneAllowed;
  result.Required := False;
  result.RadioIndex := LastSimilarSibling.RadioIndex+1;
  Frame.AssignMetaDataToTreeViewItem(result);
  AssignRadioGroupName(result);
  Frame.tvMetaData.Selected := result.TreeViewItem;
end;

function TCustomMetaDataItem.ParentItem: TCustomMetaDataItem;
var
  ParentTreeViewItem: TTreeViewItem;
begin
  result := nil;
  ParentTreeViewItem := TreeViewItem.ParentItem;
  if ParentTreeViewItem <> nil then
  begin
    result := ParentTreeViewItem.TagObject as TCustomMetaDataItem;
  end;
end;

procedure TCustomMetaDataItem.RadioButtonChecked(Sender: TObject);
var
  Item: TTreeViewItem;
  rb: TRadioButton;
begin
  rb := Sender as TRadioButton;
  Item := rb.Parent as TTreeViewItem;
  Item.IsChecked := rb.IsChecked;
end;

procedure TCustomMetaDataItem.SetCheckType(const Value: TCheckType);
begin
  FCheckType := Value;
end;

procedure TCustomMetaDataItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TCustomMetaDataItem.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TCustomMetaDataItem.SetLongName(const Value: string);
begin
  FLongName := Value;
end;

procedure TCustomMetaDataItem.SetMoreThanOneAllowed(const Value: boolean);
begin
  FMoreThanOneAllowed := Value;
end;

procedure TCustomMetaDataItem.SetRadioGroupName(const Value: string);
begin
  FRadioGroupName := Value;
end;

procedure TCustomMetaDataItem.SetRadioIndex(const Value: integer);
begin
  FRadioIndex := Value;
end;

procedure TCustomMetaDataItem.SetRequired(const Value: boolean);
begin
  FRequired := Value;
end;

procedure TCustomMetaDataItem.SetRequiredType(const Value: TRequiredType);
begin
  FRequiredType := Value;
end;

procedure TCustomMetaDataItem.SetShortName(const Value: string);
begin
  FShortName := Value;
end;

procedure TCustomMetaDataItem.SetTextHeight(const Value: single);
begin
  FHeight := Value;
end;

procedure TCustomMetaDataItem.SetUsed(const Value: Boolean);
begin
  if FUsed <> Value then
  begin
    FUsed := Value;
    if Fcb <> nil then
    begin
  //    Fcb.IsChecked := not GetUsed;
      Fcb.IsChecked := GetUsed;
    end;
    if FRb <> nil then
    begin
  //    FRb.IsChecked := not GetUsed;
      FRb.IsChecked := GetUsed;
    end;
    DoUsedChanged;
    DoChange;
  end;
end;

procedure TCustomMetaDataItem.SetXmlNode(const Value: TXmlNode);
var
  ChildIndex: Integer;
  ChildItem: TCustomMetaDataItem;
begin
  if Value = nil then
  begin
    if FXmlNode <> nil then
    begin
      for ChildIndex := 0 to TreeViewItem.Count - 1 do
      begin
        ChildItem := TreeViewItem.Items[ChildIndex].TagObject as TCustomMetaDataItem;
        ChildItem.XmlNode := nil;
      end;
      if FXmlNode.Parent <> nil then
      begin
        FXmlNode.Parent.ChildNodes.Remove(FXmlNode);
      end
      else
      begin
        FXmlNode.Free;
      end;
    end;
  end;
  FXmlNode := Value;
end;

procedure TCustomMetaDataItem.ShowFrame;
begin
  FFrame := FrameClass.Create(self);
  FFrame.Parent := Frame;
  FFrame.Align := TAlignLayout.Client;
  case Self.RequiredType of
    rtOptional: FFrame.memoColor := TAlphaColorRec.Cyan;
    rtManditoryIfApplicable: FFrame.memoColor := TAlphaColorRec.Lime;
    rtManditory: FFrame.memoColor := TAlphaColorRec.Yellow;
  end;
  FFrame.GetMetaData(self);
end;

procedure TCustomMetaDataItem.WriteChildren(Node: TXmlNode);
begin
  // do nothing
end;

{ TCompoundItemMetaDataItem }
function TCompoundItemMetaDataItem.GetNodeFromText(Node: TXmlNode): TCustomMetaDataItem;
var
  ChildIndex: Integer;
  AChild: TTreeViewItem;
  TestItem: TCustomMetaDataItem;
  TestText: string;
//  PriorItem: TCustomMetaDataItem;
//  Index: Integer;
begin
  TestText := LowerCase(Node.Name);
  result := nil;
//  Index := 0;
  for ChildIndex := 0 to TreeViewItem.Count - 1 do
  begin
    AChild := TreeViewItem.Items[ChildIndex];
    TestItem := AChild.TagObject as TCustomMetaDataItem;
    if TestItem.ShortName = TestText then
    begin
      result := TestItem;
      if not result.XmlAssigned then
      begin
        Exit;
//      end
//      else
//      begin
//        Index := ChildIndex;
      end;
    end;
  end;
  if result <> nil then
  begin
//    PriorItem := Result;
    result := result.NewItem(self);
    result.Used := True;
//    result.RadioIndex := PriorItem.RadioIndex + 1;
//    result.TreeViewItem.Index := PriorItem.TreeViewItem.Index+1;
//    result.CheckType := ctCheckBox;
//    result.MoreThanOneAllowed := PriorItem.MoreThanOneAllowed;
//    result.Required := False;
//    frmMetaData.AssignMetaDataToTreeViewItem(result);
  end
  else
  begin
    if Node.HasChildNodes then
    begin
      result := Frame.CreateUnknownCompound(self);
      result.ShortName := Node.Name;
      result.LongName := Node.Name;
      Frame.AssignMetaDataToTreeViewItem(result);
    end
    else
    begin
      result := Frame.CreateUnknownText(self);
      result.ShortName := Node.Name;
      result.LongName := Node.Name;
      Frame.AssignMetaDataToTreeViewItem(result);
    end;
  end;
end;

procedure TCompoundItemMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
var
//  XmlIndex: Integer;
  ChildNode: TXmlNode;
  ChildMetaData: TCustomMetaDataItem;
begin
  inherited;
  if Node.HasChildNodes then
  begin
    ChildNode := Node.FirstChild;
    while ChildNode <> nil do
    begin
      ChildMetaData := GetNodeFromText(ChildNode);
      ChildMetaData.AssignValuesFromXML(ChildNode);
      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;

function TCompoundItemMetaDataItem.FrameClass: TMetaDataFrameClass;
begin
  Result := TframeCustomMetaData;
end;

function TCompoundItemMetaDataItem.GetChildrenUsed: Boolean;
var
  index: Integer;
  ChildItem: TTreeViewItem;
  ChildData: ICustomMetaData;
begin
  result := False;
  for index := 0 to FItem.Count - 1 do
  begin
    ChildItem := FItem.Items[index];
    ChildData := ChildItem.TagObject as TCustomMetaDataItem;
    result := ChildData.Used;
    if Result then
    begin
      Exit;
    end;
  end;
end;

{ TNumericMetaDataItem }

procedure TNumericMetaDataItem.Assign(Source: TPersistent);
begin
  if Source is TNumericMetaDataItem then
  begin
    Content := TNumericMetaDataItem(Source).Content;
    MinValue := TNumericMetaDataItem(Source).MinValue;
    MaxValue := TNumericMetaDataItem(Source).MaxValue;
    MinLimitType := TNumericMetaDataItem(Source).MinLimitType;
    MaxLimitType := TNumericMetaDataItem(Source).MaxLimitType;
  end;
  inherited;

end;

procedure TNumericMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
var
  Default: Extended;
begin
  inherited;
  Default := Max(MinValue, 0);
  Default := Min(Default, MaxValue);
  Content := FortranStrToFloatDefault(Node.Text, Default);
end;

constructor TNumericMetaDataItem.Create(AOwner: TComponent);
begin
  inherited;
  FMin := -MaxInt;
  FMax := MaxInt;
end;

function TNumericMetaDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframeNumeric;
end;

function TNumericMetaDataItem.GetContent: double;
begin
  result := FContent;
end;

function TNumericMetaDataItem.GetMaxValue: double;
begin
  result := FMax;
end;

function TNumericMetaDataItem.GetMaxLimitType: TMaxLimitType;
begin
  Result := FMaxLimitType;
end;

function TNumericMetaDataItem.GetMinValue: double;
begin
  result := FMin;
end;

function TNumericMetaDataItem.GetMinLimitType: TMinLimitType;
begin
  result := FMinLimitType;
end;

function TNumericMetaDataItem.GetStringContent: string;
begin
  result := Content.ToString(RealSettings);
end;

procedure TNumericMetaDataItem.SetContent(const Value: double);
begin
  if FContent <> Value then
  begin
    FContent := Value;
    DoChange;
  end;
end;

procedure TNumericMetaDataItem.SetMaxValue(const Value: double);
begin
  FMax := Value;
end;

procedure TNumericMetaDataItem.SetMaxLimitType(const Value: TMaxLimitType);
begin
  FMaxLimitType := Value;
end;

procedure TNumericMetaDataItem.SetMinValue(const Value: double);
begin
  FMin := Value;
end;

procedure TNumericMetaDataItem.SetMinLimitType(const Value: TMinLimitType);
begin
  FMinLimitType := Value;
end;

{ TTextMetaDataItem }

procedure TTextMetaDataItem.Assign(Source: TPersistent);
begin
  if Source is TTextMetaDataItem then
  begin
    Content := TTextMetaDataItem(Source).Content;
  end;
  inherited;

end;

procedure TTextMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
begin
  inherited;
  Content := Node.Text;
end;

function TTextMetaDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframeTextMetaData;
end;

function TTextMetaDataItem.GetContent: String;
begin
  result := FContent;
end;

function TTextMetaDataItem.GetStringContent: string;
begin
  result := Content;
end;

procedure TTextMetaDataItem.SetContent(const Value: String);
begin
  if FContent <> Value then
  begin
    FContent := Value;
    DoChange;
  end;
end;

{ TDateMetaDataItem }

procedure TDateMetaDataItem.Assign(Source: TPersistent);
begin
  if Source is TDateMetaDataItem then
  begin
    DateFormat := TDateMetaDataItem(Source).DateFormat;
    Year := TDateMetaDataItem(Source).Year;
    Month := TDateMetaDataItem(Source).Month;
    Day := TDateMetaDataItem(Source).Day;
  end;
  inherited;

end;

procedure TDateMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
begin
  inherited;
  ConvertDate(Self, Node.Text)
end;

constructor TDateMetaDataItem.Create(AOwner: TComponent);
var
  AYear: Word;
  AMonth: Word;
  ADay: Word;
  ADate: TDateTime;
begin
  inherited;
  FDateFormat := dfYearMonthDay;
  ADate := Now;
  DecodeDate(ADate, AYear, AMonth, ADay);
  Year := AYear;
  Month := AMonth;
  Day := ADay;
end;

function TDateMetaDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframeDate;
end;

function TDateMetaDataItem.GetDateFormat: TDateFormat;
begin
  result := FDateFormat;
end;

function TDateMetaDataItem.GetDateIntf: IDate;
begin
  result := self;
end;

function TDateMetaDataItem.GetDay: integer;
begin
  result := FDay;
end;

function TDateMetaDataItem.GetMonth: integer;
begin
  result := FMonth;
end;

function TDateMetaDataItem.GetStringContent: string;
begin
  result := DateStringContent(self);
end;

function TDateMetaDataItem.GetYear: Integer;
begin
  result := FYear;
end;

procedure TDateMetaDataItem.SetDateFormat(const Value: TDateFormat);
begin
  if FDateFormat <> Value then
  begin
    FDateFormat := Value;
    DoChange;
  end;
end;

procedure TDateMetaDataItem.SetDay(const Value: integer);
begin
  if FDay <> Value then
  begin
    FDay := Value;
    DoChange;
  end;
end;

procedure TDateMetaDataItem.SetMonth(const Value: integer);
begin
  if FMonth <> Value then
  begin
    FMonth := Value;
    DoChange;
  end;
end;

procedure TDateMetaDataItem.SetYear(const Value: Integer);
begin
  if FYear <> Value then
  begin
    FYear := Value;
    DoChange;
  end;
end;

{ TChoiceMetaDataItem }

procedure TChoiceMetaDataItem.Assign(Source: TPersistent);
var
  SourceItem: TChoiceMetaDataItem;
begin
  if Source is TChoiceMetaDataItem then
  begin
    SourceItem := TChoiceMetaDataItem(Source);
    Choice := SourceItem.Choice;
    Choices := SourceItem.Choices;
  end;
  inherited;

end;

procedure TChoiceMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
begin
  inherited;
  Choice := Node.Text;
end;

constructor TChoiceMetaDataItem.Create(AOwner: TComponent);
begin
  inherited;
  FChoices := TStringList.Create;
  FAltChoices := TStringList.Create;
end;

destructor TChoiceMetaDataItem.Destroy;
begin
  FAltChoices.Free;
  FChoices.Free;
  inherited;
end;

function TChoiceMetaDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframeChoice;
end;

function TChoiceMetaDataItem.GetAltChoices: TStrings;
begin
  result := FAltChoices
end;

function TChoiceMetaDataItem.GetChoices: TStrings;
begin
  Result := FChoices;
end;

function TChoiceMetaDataItem.GetContent: string;
begin
  result := FContent;
end;

function TChoiceMetaDataItem.GetObject: TObject;
begin
  result := self;
end;

function TChoiceMetaDataItem.GetOnUpdateChoices: TNotifyEvent;
begin
  result := FOnUpdateChoices;
end;

function TChoiceMetaDataItem.GetStringContent: string;
begin
  result := Choice;
//  if (Choice >= 0) and (Choice < Choices.Count) then
//  begin
//    result := Choices[Choice];
//  end
//  else
//  begin
//    result := '';
//  end;
end;

procedure TChoiceMetaDataItem.SetAltChoices(const Value: TStrings);
begin
  FAltChoices.Assign(Value);
end;

procedure TChoiceMetaDataItem.SetChoices(const Value: TStrings);
begin
  FChoices.Assign(Value);
end;

procedure TChoiceMetaDataItem.SetContent(const Value: string);
begin
  if FContent <> Value then
  begin
    FContent := Value;
    DoChange;
  end;
end;

procedure TChoiceMetaDataItem.SetOnUpdateChoices(const Value: TNotifyEvent);
begin
  FOnUpdateChoices := Value;
end;

{ TExtentDataItem }

procedure TExtentDataItem.Assign(Source: TPersistent);
var
  SourceItem: TExtentDataItem;
begin
  if Source is TExtentDataItem then
  begin
    SourceItem := TExtentDataItem(Source);
    North := SourceItem.North;
    South := SourceItem.South;
    East := SourceItem.East;
    West := SourceItem.West;
  end;
  inherited;

end;

procedure TExtentDataItem.AssignValuesFromXML(Node: TXmlNode);
var
  ChildNode: TXmlNode;
begin
  inherited;
  if Node.HasChildNodes then
  begin
    ChildNode := Node.FirstChild;
    while ChildNode <> nil do
    begin
      if LowerCase(ChildNode.Name) = 'westbc' then
      begin
        West := FortranStrToFloatDefault(ChildNode.Text, 0);
      end
      else if LowerCase(ChildNode.Name) = 'eastbc' then
      begin
        East := FortranStrToFloatDefault(ChildNode.Text, 0);
      end
      else if LowerCase(ChildNode.Name) = 'northbc' then
      begin
        North := FortranStrToFloatDefault(ChildNode.Text, 0);
      end
      else if LowerCase(ChildNode.Name) = 'southbc' then
      begin
        South := FortranStrToFloatDefault(ChildNode.Text, 0);
      end;
      ChildNode := ChildNode.NextSibling;
    end;
  end;

end;

function TExtentDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframeExtent
end;

function TExtentDataItem.GetEast: double;
begin
  result := FEast;
end;

function TExtentDataItem.GetNorth: double;
begin
  result := FNorth;
end;

function TExtentDataItem.GetSouth: double;
begin
  result := FSouth;
end;

function TExtentDataItem.GetWest: double;
begin
  result := FWest;
end;

procedure TExtentDataItem.SetEast(const Value: double);
begin
  if FEast <> Value then
  begin
    FEast := Value;
    DoChange;
  end;
end;

procedure TExtentDataItem.SetNorth(const Value: double);
begin
  if FNorth <> Value then
  begin
    FNorth := Value;
    DoChange;
  end;
end;

procedure TExtentDataItem.SetSouth(const Value: double);
begin
  if FSouth <> Value then
  begin
    FSouth := Value;
    DoChange;
  end;
end;

procedure TExtentDataItem.SetWest(const Value: double);
begin
  if FWest <> Value then
  begin
    FWest := Value;
    DoChange;
  end;
end;

procedure TExtentDataItem.WriteChildren(Node: TXmlNode);
var
  NewNode: TXmlNode;
  function GetChildNode(const NodeText: string): TXmlNode;
  begin
    Result := Node.Find(NodeText);
    if result = nil then
    begin
      result := Node.AddChild(NodeText);
    end;
  end;
begin
  inherited;
  NewNode := GetChildNode('westbc');
  NewNode.Text := West.ToString(RealSettings);

  NewNode := GetChildNode('eastbc');
  NewNode.Text := East.ToString(RealSettings);

  NewNode := GetChildNode('northbc');
  NewNode.Text := North.ToString(RealSettings);

  NewNode := GetChildNode('southbc');
  NewNode.Text := South.ToString(RealSettings);
end;

{ TChoicePlusMetaDataItem }

procedure TChoicePlusMetaDataItem.Assign(Source: TPersistent);
begin
  if Source is TChoicePlusMetaDataItem then
  begin
    FreeContent := TChoicePlusMetaDataItem(Source).FreeContent;
  end;
  inherited;
end;

procedure TChoicePlusMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
begin
  inherited;
  if Choices.IndexOf(Choice) < 0 then
  begin
    FreeContent := Node.Text;
  end;
end;

function TChoicePlusMetaDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframeChoicePlus;
end;

function TChoicePlusMetaDataItem.GetFreeContent: string;
begin
  Result := FFreeContent;
end;

function TChoicePlusMetaDataItem.GetStringContent: string;
begin
  if FFreeContent = '' then
  begin
    result := inherited GetStringContent
  end
  else
  begin
    result := FFreeContent;
  end;
end;

procedure TChoicePlusMetaDataItem.SetFreeContent(const Value: string);
begin
  if FFreeContent <> Value then
  begin
    FFreeContent := Value;
    DoChange;
  end;
end;

{ TChoicePlusDateMetaDataItem }

//procedure TChoicePlusDateMetaDataItem.Assign(Source: TPersistent);
//begin
//  if Source is TChoicePlusDateMetaDataItem then
//  begin
//    Date := TChoicePlusDateMetaDataItem(Source).Date;
//  end;
//  inherited;
//end;

procedure TChoicePlusDateMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
begin
  inherited;
  if not ConvertDate(self, Node.Text) then
  begin
    Choice := Node.Text;
  end;
end;

constructor TChoicePlusDateMetaDataItem.Create(AOwner: TComponent);
var
  AYear: Word;
  AMonth: Word;
  ADay: Word;
  ADate: TDateTime;
begin
  inherited;
  FDateFormat := dfYearMonthDay;
  ADate := Now;
  DecodeDate(ADate, AYear, AMonth, ADay);
  Year := AYear;
  Month := AMonth;
  Day := ADay;
end;

function TChoicePlusDateMetaDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframeChoicePlusDate;
end;

function TChoicePlusDateMetaDataItem.GetDateFormat: TDateFormat;
begin
  result := FDateFormat;
end;

function TChoicePlusDateMetaDataItem.GetDateIntf: IDate;
begin
  result := self;
end;

function TChoicePlusDateMetaDataItem.GetDay: integer;
begin
  result := FDay;
end;

function TChoicePlusDateMetaDataItem.GetMonth: integer;
begin
  result := FMonth;
end;

function TChoicePlusDateMetaDataItem.GetStringContent: string;
//var
//  Settings: TFormatSettings;
begin
  if Choice <> '' then
  begin
    Result := Choice;
  end
  else
  begin
    Result := DateStringContent(self);
  end;
end;

function TChoicePlusDateMetaDataItem.GetYear: Integer;
begin
  result := FYear;
end;

procedure TChoicePlusDateMetaDataItem.SetDateFormat(const Value: TDateFormat);
begin
  if FDateFormat <> Value then
  begin
    FDateFormat := Value;
    DoChange;
  end;
end;

procedure TChoicePlusDateMetaDataItem.SetDay(const Value: integer);
begin
  if FDay <> Value then
  begin
    FDay := Value;
    DoChange;
  end;
end;

procedure TChoicePlusDateMetaDataItem.SetMonth(const Value: integer);
begin
  if FMonth <> Value then
  begin
    FMonth := Value;
    DoChange;
  end;
end;

procedure TChoicePlusDateMetaDataItem.SetYear(const Value: Integer);
begin
  if FYear <> Value then
  begin
    FYear := Value;
    DoChange;
  end;
end;

{ TPolyPoint }
{ TPolygon }
{ TPolygonMetaDataItem }

procedure TPolygonMetaDataItem.Assign(Source: TPersistent);
begin
  if Source is TPolygonMetaDataItem then
  begin
    Polygon := TPolygonMetaDataItem(Source).Polygon;
  end;
  inherited;

end;

procedure TPolygonMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
var
  ChildNode: TXmlNode;
  Splitter: TStringList;
  PointIndex: Integer;
  APoint: TPolyPoint;
  procedure ReadAPoint(PointNode: TXmlNode);
  var
    CNode: TXmlNode;
  begin
    if PointNode.HasChildNodes then
    begin
      APoint := Polygon.Add;
      CNode := PointNode.FirstChild;
      while CNode <> nil do
      begin
        if LowerCase(CNode.Name) = 'gringlat' then
        begin
          APoint.Latitude := FortranStrToFloatDefault(CNode.Text, 0);
        end
        else if LowerCase(CNode.Name) = 'gringlon' then
        begin
          APoint.Longitude := FortranStrToFloatDefault(CNode.Text, 0);
        end;
        CNode := CNode.NextSibling;
      end;
    end;
  end;
begin
  inherited;
  if Node.HasChildNodes then
  begin
    ChildNode := Node.FirstChild;
    while ChildNode <> nil do
    begin
      if LowerCase(ChildNode.Name) = 'grngpoin' then
      begin
        ReadAPoint(ChildNode);
      end
      else if LowerCase(ChildNode.Name) = 'gring' then
      begin
        Splitter := TStringList.Create;
        try
          Splitter.CommaText := ChildNode.Text;
          APoint := nil;
          for PointIndex := 0 to Splitter.Count - 1 do
          begin
            if not Odd(PointIndex) then
            begin
              APoint := Polygon.Add;
            end;
            Assert(APoint <> nil);
            if Odd(PointIndex) then
            begin
              APoint.Latitude :=
                FortranStrToFloatDefault(Splitter[PointIndex], 0);
            end
            else
            begin
              APoint.Longitude :=
                FortranStrToFloatDefault(Splitter[PointIndex], 0);
            end;
          end;
        finally
          Splitter.Free;
        end;
      end;
      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;

constructor TPolygonMetaDataItem.Create(AOwner: TComponent);
begin
  inherited;
  FPolygon := TPolygon.Create;
end;

destructor TPolygonMetaDataItem.Destroy;
begin
  FPolygon.Free;
  inherited;
end;

function TPolygonMetaDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframePolygon
end;

function TPolygonMetaDataItem.GetPolygon: TPolygon;
begin
  result := FPolygon;
end;

procedure TPolygonMetaDataItem.SetPolygon(const Value: TPolygon);
begin
  if not FPolygon.IsSame(Value) then
  begin
    FPolygon.Assign(Value);
    DoChange;
  end;
end;

procedure TPolygonMetaDataItem.WriteChildren(Node: TXmlNode);
var
  PolyIndex: Integer;
  APoint: TPolyPoint;
  PolygonPointNode: TXmlNode;
  PolyLatitude: TXmlNode;
  PolyLongitude: TXmlNode;
//  NodeList: TXmlNode;
  NodeIndex: Integer;
  NodeList: TXmlNodeList;
begin
  inherited;

  NodeList := Node.FindNodes('grngpoin');
  try
    for NodeIndex := NodeList.Count - 1 downto Polygon.Count do
    begin
      NodeList[NodeIndex].Free;
    end;
    for PolyIndex := 0 to Polygon.Count - 1 do
    begin
      APoint := Polygon[PolyIndex];
      if PolyIndex < NodeList.Count then
      begin
        PolygonPointNode := NodeList[PolyIndex];
      end
      else
      begin
        PolygonPointNode := Node.AddChild('grngpoin');
      end;
      PolyLatitude := PolygonPointNode.Find('gringlat');
      if PolyLatitude = nil then
      begin
        PolyLatitude := PolygonPointNode.AddChild('gringlat');
      end;
      PolyLatitude.Text := APoint.Latitude.ToString(RealSettings);

      PolyLongitude := PolygonPointNode.Find('gringlon');
      if PolyLongitude = nil then
      begin
        PolyLongitude := PolygonPointNode.AddChild('gringlon');
      end;
      PolyLongitude.Text := APoint.Longitude.ToString(RealSettings);
    end;
  finally
    NodeList.Free;
  end;
end;

{ TIntegerMetaDataItem }

procedure TIntegerMetaDataItem.AddIllegalValue(Value: Integer);
begin
  FIllegalValues.Add(Value);
end;

procedure TIntegerMetaDataItem.Assign(Source: TPersistent);
var
  SourcItem: TIntegerMetaDataItem;
begin
  if Source is TIntegerMetaDataItem then
  begin
    SourcItem := TIntegerMetaDataItem(Source);
    Content := SourcItem.Content;
    MinValue := SourcItem.MinValue;
    MaxValue := SourcItem.MaxValue;
  end;
  inherited;

end;

procedure TIntegerMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
var
  Default: Integer;
begin
  inherited;
  Default := Max(MinValue, 0);
  Default := Min(Default, MaxValue);
  Content := StrToIntDef(Node.Text, Default);
end;

constructor TIntegerMetaDataItem.Create(AOwner: TComponent);
begin
  inherited;
  FMax := MaxInt;
  FMin := -MaxInt;
  FIllegalValues := TList<Integer>.Create;
end;

destructor TIntegerMetaDataItem.Destroy;
begin
  FIllegalValues.Free;
  inherited;
end;

function TIntegerMetaDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframeIntegerMetaData
end;

function TIntegerMetaDataItem.GetContent: integer;
begin
  result := FContent;
end;

function TIntegerMetaDataItem.GetIllegalValue(Index: Integer): Integer;
begin
  result := FIllegalValues[index];
end;

function TIntegerMetaDataItem.GetMaxValue: Integer;
begin
  result := FMax;
end;

function TIntegerMetaDataItem.GetMinValue: Integer;
begin
  result := FMin;
end;

function TIntegerMetaDataItem.GetStringContent: string;
begin
  result := FContent.ToString;
end;

function TIntegerMetaDataItem.IllegalValueCount: Integer;
begin
  result := FIllegalValues.Count;
end;

procedure TIntegerMetaDataItem.SetContent(const Value: integer);
begin
  if FContent <> Value then
  begin
    FContent := Value;
    DoChange;
  end;
end;

procedure TIntegerMetaDataItem.SetMaxValue(const Value: Integer);
begin
  FMax := Value;
end;

procedure TIntegerMetaDataItem.SetMinValue(const Value: Integer);
begin
  FMin := Value;
end;

{ TTimeMetaDataItem }

procedure TTimeMetaDataItem.AssignValuesFromXML(Node: TXmlNode);
begin
  inherited;
  ConvertTimeStr(self, Node.Text);
end;

function TTimeMetaDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframeTime;
end;

function TTimeMetaDataItem.GetDifferentialHours: integer;
begin
  result := FDifferentialHours;
end;

function TTimeMetaDataItem.GetDifferentialMinutes: Integer;
begin
  result := FDifferentialMinutes;
end;

function TTimeMetaDataItem.GetFormatChoice: TTimeChoice;
begin
  result := FFormatChoice;
end;

function TTimeMetaDataItem.GetStringContent: string;
//var
//  SignSym: String;
begin
  result := TimeToMetaStr(self);
//  case FormatChoice of
//    tcLocal:
//      begin
//        Result := FormatDateTime('hhnnsszzz', Time);
//      end;
//    tcLocalDifferential:
//      begin
//        if DifferentialHours >= 0 then
//        begin
//          SignSym := '+';
//        end
//        else
//        begin
//          SignSym := '-';
//        end;
//        Result := FormatDateTime('hhnnsszzz', Time) + SignSym
//          + Format('%0:.2d%1:.2d', [DifferentialHours, DifferentialMinutes]);
//      end;
//    tcUniversal:
//      begin
//        Result := FormatDateTime('hhnnsszzz', Time) + 'Z';
//      end;
//  end;
end;

function TTimeMetaDataItem.GetTime: TTime;
begin
  result := FTime;
end;

function TTimeMetaDataItem.GetTimeIntf: ITime;
begin
  result := self;
end;

procedure TTimeMetaDataItem.SetDifferentialHours(const Value: integer);
begin
  if FDifferentialHours <> Value then
  begin
    FDifferentialHours := Value;
    DoChange;
  end;
end;

procedure TTimeMetaDataItem.SetDifferentialMinutes(const Value: Integer);
begin
  if FDifferentialMinutes <> Value then
  begin
    FDifferentialMinutes := Value;
    DoChange;
  end;
end;

procedure TTimeMetaDataItem.SetFormatChoice(const Value: TTimeChoice);
begin
  if FFormatChoice <> Value then
  begin
    FFormatChoice := Value;
    DoChange;
  end;
end;

procedure TTimeMetaDataItem.SetTime(const Value: TTime);
begin
  if FTime <> Value then
  begin
    FTime := Value;
    DoChange;
  end;
end;

{ TChoicePlusTimeDataItem }

procedure TChoicePlusTimeDataItem.AssignValuesFromXML(Node: TXmlNode);
begin
  inherited;
  if not ConvertTimeStr(self, Node.Text) then
  begin
    Choice := Node.Text;
  end;
end;

function TChoicePlusTimeDataItem.FrameClass: TMetaDataFrameClass;
begin
  result := TframeChoicePlusTime;
end;

function TChoicePlusTimeDataItem.GetDifferentialHours: integer;
begin
  result := FDifferentialHours;
end;

function TChoicePlusTimeDataItem.GetDifferentialMinutes: Integer;
begin
  result := FDifferentialMinutes;
end;

function TChoicePlusTimeDataItem.GetFormatChoice: TTimeChoice;
begin
  result := FFormatChoice;
end;

function TChoicePlusTimeDataItem.GetStringContent: string;
//var
//  SignSym: String;
begin
  if Choice <> '' then
  begin
    Result := Choice;
  end
  else
  begin
    result := TimeToMetaStr(self);

//    case FormatChoice of
//      tcLocal:
//        begin
//          Result := FormatDateTime('hhnnsszzz', Time);
//        end;
//      tcLocalDifferential:
//        begin
//          if DifferentialHours >= 0 then
//          begin
//            SignSym := '+';
//          end
//          else
//          begin
//            SignSym := '';
//          end;
//          Result := FormatDateTime('hhnnsszzz', Time) + SignSym
//            + Format('%0:.2d%1:.2d', [DifferentialHours, DifferentialMinutes]);
//        end;
//      tcUniversal:
//        begin
//          Result := FormatDateTime('hhnnsszzz', Time) + 'Z';
//        end;
//    end;
  end;
end;

function TChoicePlusTimeDataItem.GetTime: TTime;
begin
  result := FTime;
end;

function TChoicePlusTimeDataItem.GetTimeIntf: ITime;
begin
  result := self;
end;

procedure TChoicePlusTimeDataItem.SetDifferentialHours(const Value: integer);
begin
  if FDifferentialHours <> Value then
  begin
    FDifferentialHours := Value;
    DoChange;
  end;
end;

procedure TChoicePlusTimeDataItem.SetDifferentialMinutes(const Value: Integer);
begin
  if FDifferentialMinutes <> Value then
  begin
    FDifferentialMinutes := Value;
    DoChange;
  end;
end;

procedure TChoicePlusTimeDataItem.SetFormatChoice(const Value: TTimeChoice);
begin
  if FFormatChoice <> Value then
  begin
    FFormatChoice := Value;
    DoChange;
  end;
end;

procedure TChoicePlusTimeDataItem.SetTime(const Value: TTime);
begin
  if FTime <> Value then
  begin
    FTime := Value;
    DoChange;
  end;
end;

initialization

  RegisterFmxClasses([TCustomMetaDataItem, TCompoundItemMetaDataItem,
    TNumericMetaDataItem, TTextMetaDataItem, TChoiceMetaDataItem,
    TChoicePlusMetaDataItem, TDateMetaDataItem, TExtentDataItem]);

  RealSettings := TFormatSettings.Create;
  RealSettings.DecimalSeparator := '.';

end.
