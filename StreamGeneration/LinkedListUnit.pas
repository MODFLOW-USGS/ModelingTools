unit LinkedListUnit;

interface

type
  TLinkedList<T> = class(TObject)
  private
    FData: T;
    FNext: TLinkedList<T>;
    procedure SetData(const Value: T);
    procedure SetNext(const Value: TLinkedList<T>);
  public
    property Data: T read FData write SetData;
    property Next: TLinkedList<T> read FNext write SetNext;
  end;

  TDoubleLinkedList<T> = class(TObject)
  private
    FData: T;
    FPrevious: TDoubleLinkedList<T>;
    FNext: TDoubleLinkedList<T>;
    procedure SetData(const Value: T);
    procedure SetNext(const Value: TDoubleLinkedList<T>);
    procedure SetPrevious(const Value: TDoubleLinkedList<T>);
  public
    property Data: T read FData write SetData;
    property Next: TDoubleLinkedList<T> read FNext write SetNext;
    property Previous: TDoubleLinkedList<T> read FPrevious write SetPrevious;
  end;

implementation

{ TLinkedList<T> }

procedure TLinkedList<T>.SetData(const Value: T);
begin
  FData := Value;
end;

procedure TLinkedList<T>.SetNext(const Value: TLinkedList<T>);
begin
  FNext := Value;
end;

{ TDoubleLinkedList<T> }

procedure TDoubleLinkedList<T>.SetData(const Value: T);
begin
  FData := Value;
end;

procedure TDoubleLinkedList<T>.SetNext(const Value: TDoubleLinkedList<T>);
begin
  FNext := Value;
end;

procedure TDoubleLinkedList<T>.SetPrevious(const Value: TDoubleLinkedList<T>);
begin
  FPrevious := Value;
end;

end.
