unit UTagRepository;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, UTag;

type

  TTagRepository = class
  private
    FSQLConnection: TSQLConnection;
    FSQLTransaction: TSQLTransaction;
  public
    constructor Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction);
    function GetTags: TTagList;
  end;


implementation

constructor TTagRepository.Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction);
begin
  inherited Create;
  FSQLConnection:= AConnection;
  FSQLTransaction:= ATransaction;
end;

function TTagRepository.GetTags: TTagList;
var
  SQLQuery: TSQLQuery;
begin
  Result:= TTagList.Create;

  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;

    SQLQuery.SQL.Text:= 'SELECT id, label FROM tags;';
    SQLQuery.Open;

    while not SQLQuery.EOF do
    begin
      Result.Add(TTag.Create(SQLQuery.FieldByName('id').AsInteger,
                             SQLQuery.FieldByName('label').AsString));
      SQLQuery.Next;
    end;
  finally
    SQLQuery.Free;
  end;
end;


end.

