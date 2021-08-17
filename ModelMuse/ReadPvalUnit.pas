unit ReadPvalUnit;

interface

uses
  Generics.Collections, IOUtils, Types;

type
  TParamItem = class(TObject)
    Name: string;
    Value: Double;
  end;

  TParamList = TObjectList<TParamItem>;

function ReadPvalFile(const FileName: string; List: TParamList): boolean;
function ReadParFile(const FileName: string; List: TParamList): boolean;

implementation

uses
  SysUtils, ModelMuseUtilities, System.Classes;

function ReadPvalFile(const FileName: string; List: TParamList): boolean;
var
  Lines: TStringDynArray;
  Count: Integer;
  Index: Integer;
  AName: string;
  Value: string;
  AValue: Extended;
  Sep: Char;
  Item: TParamItem;
  LineIndex: Integer;
  ALine: string;
  StartLine: Integer;
  SpacePos: integer;
begin
  result := False;
  if (TFile.Exists(FileName)) then
  begin
    Lines := TFile.ReadAllLines(FileName);

    if Length(Lines) > 0 then
    begin
      StartLine := Length(Lines);
      for LineIndex := 0 to Length(Lines) - 1 do
      begin
        ALine := Lines[LineIndex];
        if (Length(ALine) > 0) and (ALine[1] <> '#') then
        begin
          StartLine := LineIndex;
          break;
        end;
      end;
      if StartLine < Length(Lines) then
      begin
        if TryStrToInt(Lines[StartLine], Count) then
        begin
          result := True;
          Sep := FormatSettings.DecimalSeparator;
          try
            FormatSettings.DecimalSeparator := '.';
            for Index := 1 to Count do
            begin
              if StartLine+Index >= Length(Lines) then
              begin
                break;
              end;
              ALine := Lines[StartLine+Index];
              if Trim(ALine) = '' then
              begin
                Continue;
              end;
              SpacePos := Pos(' ', ALine);
              if SpacePos > 10 then
              begin
                SpacePos := 10;
              end;
              if SpacePos = 0 then
              begin
                SpacePos := Pos(#9, ALine);
                if SpacePos > 10 then
                begin
                  SpacePos := 10;
                end;
              end;
              AName := Trim(Copy(ALine, 1, SpacePos));
              Value := Trim(Copy(ALine, SpacePos+1, MaxInt));
              if TryFortranStrToFloat(Value, AValue) then
              begin
                Item := TParamItem.Create;
                Item.Name := AName;
                Item.Value := AValue;
                List.Add(Item);
              end
              else
              begin
                result := False;
                Exit;
              end;
            end;

            for LineIndex := StartLine+Count+1 to Length(Lines) - 1 do
            begin
              ALine := Lines[LineIndex];
              if Trim(ALine) = '' then
              begin
                Continue;
              end;
              if Pos('#--', ALine) = 1 then
              begin
                ALine := Trim(Copy(ALine, 4, MAXINT));
              end
              else
              begin
                Continue
              end;
              SpacePos := Pos(' ', ALine);
              if SpacePos > 10 then
              begin
                SpacePos := 10;
              end;
              if SpacePos = 0 then
              begin
                SpacePos := Pos(#9, ALine);
                if SpacePos > 10 then
                begin
                  SpacePos := 10;
                end;
              end;
              AName := Trim(Copy(ALine, 1, SpacePos));
              Value := Trim(Copy(ALine, SpacePos+1, MaxInt));
              if TryFortranStrToFloat(Value, AValue) then
              begin
                Item := TParamItem.Create;
                Item.Name := AName;
                Item.Value := AValue;
                List.Add(Item);
              end
              else
              begin
                result := False;
                Exit;
              end;
            end;
          finally
            FormatSettings.DecimalSeparator := Sep;
          end;
        end;
      end;
    end;
  end;
end;

function ReadParFile(const FileName: string; List: TParamList): boolean;
var
  Lines: TStringDynArray;
  LineIndex: Integer;
  Splitter: TStringList;
  AName: string;
  Value: string;
  AValue: Extended;
  Item: TParamItem;
begin
  result := False;
  if (TFile.Exists(FileName)) then
  begin
    Lines := TFile.ReadAllLines(FileName);
    if Length(Lines) > 0 then
    begin
      result := True;
      Splitter := TStringList.Create;
      try
        for LineIndex := 1 to Length(Lines) - 1 do
        begin
          Splitter.DelimitedText := Trim(Lines[LineIndex]);
          if Splitter.Count >= 2 then
          begin
            AName := Splitter[0];
            Value := Splitter[1];
            if TryFortranStrToFloat(Value, AValue) then
            begin
              Item := TParamItem.Create;
              Item.Name := AName;
              Item.Value := AValue;
              List.Add(Item);
            end
            else
            begin
              result := False;
              Exit;
            end;
          end;
        end;
      finally
        Splitter.Free;
      end;
    end;
  end;

end;

end.
