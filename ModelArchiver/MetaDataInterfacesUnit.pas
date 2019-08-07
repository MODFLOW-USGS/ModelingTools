unit MetaDataInterfacesUnit;

interface

uses
  System.Classes;

type
  TDateFormat = (dfYear, dfYearMonth, dfYearMonthDay);

  ICustomMetaData = interface ['{C252D58C-FB29-4C95-B806-DF30FB2EE50F}']
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
    function GetStringContent: string;
    function GetTextHeight: single;
    procedure SetTextHeight(const Value: single);
    // @name describes what the item represents
    property Description: string read GetDescription write SetDescription;
    // @name is the term used for the item in the XML file.
    property ShortName: string read GetShortName write SetShortName;
    // @name records whether or not an item should be included in the metadata.
    // If any children of an item should be included, it must be included too.
    property Used: Boolean read GetUsed write SetUsed;
    // If @name is true, the item must be included in the metadata.
    property Required: boolean read GetRequired write SetRequired;
    // @name is the numeric text used to refer to the item in
    // https://www.fgdc.gov/metadata/csdgm. For example: "1.2.3".
    property ID: string read GetID write SetID;
    property TextHeight: single read GetTextHeight write SetTextHeight;
  end;

  TMinLimitType = (mltGreaterEqual, mltGreater);
  TMaxLimitType = (mltLessEqual, mltLess);

  INumericInterface = interface (ICustomMetaData) ['{F80001E7-B24D-416B-852A-AB0061E7EB39}']
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
    property Content: double read GetContent write SetContent;
    property MinValue: double read GetMinValue write SetMinValue;
    property MaxValue: double read GetMaxValue write SetMaxValue;
    property MinLimitType: TMinLimitType read GetMinLimitType write SetMinLimitType;
    property MaxLimitType: TMaxLimitType read GetMaxLimitType write SetMaxLimitType;
  end;

  IIntegerInterface = interface (ICustomMetaData) ['{3419C900-1CD3-4DE8-94E9-678E4606C1E9}']
    function GetMaxValue: Integer;
    function GetMinValue: Integer;
    procedure SetMaxValue(const Value: Integer);
    procedure SetMinValue(const Value: Integer);
    procedure SetContent(const Value: Integer);
    function GetContent: Integer;
    property Content: Integer read GetContent write SetContent;
    property MinValue: Integer read GetMinValue write SetMinValue;
    property MaxValue: Integer read GetMaxValue write SetMaxValue;
    function IllegalValueCount: Integer;
    function GetIllegalValue(Index: Integer): Integer;
    property IllegalValues[Index: Integer]: Integer read GetIllegalValue;
  end;

  ITextMetaDataItem = interface (ICustomMetaData) ['{96DF1277-0F77-4C0C-9018-625CBDD464F7}']
    procedure SetContent(const Value: String);
    function GetContent: String;
    property Content: String read GetContent write SetContent;
  end;

  IChoiceMetaDataItem = interface (ICustomMetaData) ['{CFD22B9B-4833-475A-A6B9-CBD462A98B32}']
    procedure SetContent(const Value: string);
    function GetContent: string;
    procedure SetChoices(const Value: TStrings);
    function GetChoices: TStrings;
    function GetAltChoices: TStrings;
    procedure SetAltChoices(const Value: TStrings);
    function GetOnUpdateChoices: TNotifyEvent;
    procedure SetOnUpdateChoices(const Value: TNotifyEvent);
    property Choice: string read GetContent write SetContent;
    property Choices: TStrings read GetChoices write SetChoices;
    property AlternativeChoices: TStrings read GetAltChoices write SetAltChoices;
    property OnUpdateChoices: TNotifyEvent read GetOnUpdateChoices
      write SetOnUpdateChoices;
    function GetObject: TObject;
  end;

  IChoicePlusMetaDataItem = interface (IChoiceMetaDataItem) ['{D2887BA9-848A-430B-AE8F-6F6EE96A49C5}']
    function GetFreeContent: string;
    procedure SetFreeContent(const Value: string);
    property FreeContent: string read GetFreeContent write SetFreeContent;
  end;

  IDate = interface ['{B7A7A51B-2D6C-4A0C-8B3D-90D252190014}']
//    procedure SetContent(const Value: TDate);
//    function GetContent: TDate;
    function GetDateFormat: TDateFormat;
    procedure SetDateFormat(const Value: TDateFormat);
    function GetYear: Integer;
    procedure SetYear(const Value: Integer);
    function GetDay: integer;
    function GetMonth: integer;
    procedure SetDay(const Value: integer);
    procedure SetMonth(const Value: integer);
//    property Content: TDate read GetContent write SetContent;
    property DateFormat: TDateFormat read GetDateFormat write SetDateFormat;
    property Year: Integer read GetYear write SetYear;
    property Month: integer read GetMonth write SetMonth;
    property Day: integer read GetDay write SetDay;
  end;

  IChoicePlusDateMetaDataItem = interface (IChoiceMetaDataItem) ['{6BC77241-033C-419F-B8AF-CE8B3CF591F2}']
    function GetDateIntf: IDate;
    property DateIntf: IDate read GetDateIntf;
  end;

  IDateMetaDataItem = interface (ICustomMetaData) ['{40F9DDDB-CFF6-474A-B072-CD700CFB7872}']
    function GetDateIntf: IDate;
    property DateIntf: IDate read GetDateIntf;
  end;

  IExtentDataItem = interface (ICustomMetaData) ['{5C8FFA65-2C4B-4911-A9DC-3898044AE1DF}']
    function GetEast: double;
    function GetNorth: double;
    function GetSouth: double;
    function GetWest: double;
    procedure SetEast(const Value: double);
    procedure SetNorth(const Value: double);
    procedure SetSouth(const Value: double);
    procedure SetWest(const Value: double);
    property North: double read GetNorth write SetNorth;
    property South: double read GetSouth write SetSouth;
    property East: double read GetEast write SetEast;
    property West: double read GetWest write SetWest;
  end;

  TPolyPoint = class(TCollectionItem)
  private
    FX: double;
    FY: double;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Longitude: double read FX write FX;
    property Latitude: double read FY write FY;
  end;

  TPolygon = class(TCollection)
  private
    function GetItem(index: Integer): TPolyPoint;
    procedure SetItem(index: Integer; const Value: TPolyPoint);
  public
    constructor Create;
    property Items[index: Integer]: TPolyPoint read GetItem
      write SetItem; default;
    function Add: TPolyPoint;
    function IsSame(AnotherPolygon: TPolygon): boolean;
  end;

  IPolygonDataItem = interface (ICustomMetaData) ['{EF39B66E-6C90-48FE-B09F-8A4EFBCCEC4D}']
    procedure SetPolygon(const Value: TPolygon);
    function GetPolygon: TPolygon;
    property Polygon: TPolygon read GetPolygon write SetPolygon;
  end;

  TTimeChoice = (tcLocal, tcLocalDifferential, tcUniversal);

  ITime = Interface ['{3B55466B-D185-4143-AC0D-5843E35945C3}']
    function GetDifferentialHours: integer;
    function GetDifferentialMinutes: Integer;
    function GetFormatChoice: TTimeChoice;
    function GetTime: TTime;
    procedure SetDifferentialHours(const Value: integer);
    procedure SetDifferentialMinutes(const Value: Integer);
    procedure SetFormatChoice(const Value: TTimeChoice);
    procedure SetTime(const Value: TTime);
    property Time: TTime read GetTime write SetTime;
    property FormatChoice: TTimeChoice read GetFormatChoice write SetFormatChoice;
    property DifferentialHours: integer read GetDifferentialHours write SetDifferentialHours;
    property DifferentialMinutes: Integer read GetDifferentialMinutes write SetDifferentialMinutes;
  End;

  ITimeDataItem = interface (ICustomMetaData) ['{F2D1A11D-DCBB-4EBF-9F4E-F4B5372A4DD0}']
    function GetTimeIntf: ITime;
    property TimeIntf: ITime read GetTimeIntf;
  end;

  IChoicePlusTimeMetaDataItem = interface (IChoiceMetaDataItem) ['{9D101A23-383C-4518-AD21-CE3FAE382E9C}']
    function GetTimeIntf: ITime;
    property TimeIntf: ITime read GetTimeIntf;
  end;

implementation

procedure TPolyPoint.Assign(Source: TPersistent);
var
  SourcePoint: TPolyPoint;
begin
  if Source is TPolyPoint then
  begin
    SourcePoint := TPolyPoint(Source);
    Longitude := SourcePoint.Longitude;
    Latitude := SourcePoint.Latitude;
  end
  else
  begin
    inherited;
  end;
end;

function TPolygon.Add: TPolyPoint;
begin
  result := inherited Add as TPolyPoint;
end;

constructor TPolygon.Create;
begin
  inherited Create(TPolyPoint);
end;

function TPolygon.GetItem(index: Integer): TPolyPoint;
begin
  result := inherited Items[Index] as TPolyPoint
end;

function TPolygon.IsSame(AnotherPolygon: TPolygon): boolean;
var
  ItemIndex: Integer;
  Item: TPolyPoint;
  AnotherPoint: TPolyPoint;
begin
  result := Count = AnotherPolygon.Count;
  if result then
  begin
    for ItemIndex := 0 to Count - 1 do
    begin
      Item := Items[ItemIndex];
      AnotherPoint := AnotherPolygon[ItemIndex];
      Result := (Item.Longitude = AnotherPoint.Longitude)
        and  (Item.Latitude = AnotherPoint.Latitude);
      if not result then
      begin
        Break;
      end;
    end;
  end;
end;

procedure TPolygon.SetItem(index: Integer; const Value: TPolyPoint);
begin
  inherited Items[index] := Value;
end;

end.
