unit PestPropertiesUnit;

interface

uses
  System.Classes, GoPhastTypes;

type
  TPestProperties = class(TGoPhastPersistent)
  private
    FTemplateCharacter: Char;
    FExtendedTemplateCharacter: Char;
    procedure SetTemplateCharacter(const Value: Char);
    procedure SetExtendedTemplateCharacter(const Value: Char);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
  Published
    property TemplateCharacter: Char read FTemplateCharacter
      write SetTemplateCharacter;
    property ExtendedTemplateCharacter: Char read FExtendedTemplateCharacter write SetExtendedTemplateCharacter;
  end;

implementation

{ TPestProperties }

constructor TPestProperties.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FTemplateCharacter := '@';
  FExtendedTemplateCharacter := '!';
end;

procedure TPestProperties.SetExtendedTemplateCharacter(const Value: Char);
begin
  SetCharacterProperty(FExtendedTemplateCharacter, Value);
end;

procedure TPestProperties.SetTemplateCharacter(const Value: Char);
begin
  SetCharacterProperty(FTemplateCharacter, Value);
end;

end.
