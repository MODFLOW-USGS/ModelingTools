unit DataItemUnit;

{ Code to assist writing of data to text files }

interface

  uses GlobalData, GlobalTypesUnit, SysUtils;

  type

    TDataItem = class(TObject)
      private
      protected
      public
        ItemType: TItemType;
        function WriteItem(Width: integer): string; virtual; abstract;
        function FortranDecimal(NumberString: string): string;
    end;

    TDoubleItem = class(TDataItem)
      private
        fValue: double;
      protected
      public
        property Value: double read fValue write fValue;
        function WriteItem(Width: integer): string; override;
    end;

    TStringItem = class(TDataItem)
      private
        fValue: string;
      protected
      public
        property Value: string read fValue write fValue;
        function WriteItem(Width: integer): string; override;
    end;

implementation

{ TDataItem }

function TDataItem.FortranDecimal(NumberString: string): string;
begin
  if DecimalSeparator = '.' then
  begin
    result := NumberString;
  end
  else
  begin
    result := StringReplace(NumberString, DecimalSeparator, '.',
      [rfReplaceAll]);
  end;
end;

{ TDoubleItem }

function TDoubleItem.WriteItem(Width: integer): string;
var
  Index : integer;
  PadIndex : integer;
begin
  for Index := Width downto 1 do
  begin
    result := Format(' %.*g', [Index, Value]);
    if Length(result) <= Width then
    begin
      for PadIndex := 0 to Width - Length(result) -1 do
      begin
        result := ' ' + result;
      end;
      break;
    end;
  end;
  result := FortranDecimal(result);
end;

{ TStringItem }

function TStringItem.WriteItem(Width: integer): string;
begin
  result := Value;
end;

end.
