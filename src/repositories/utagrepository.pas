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
    function Add(const ATag: string): TTag;
    procedure Update(const AId: Integer; const ATag: string);
    procedure Delete(const AId: Integer);
    function GetTags: TTagList;
  end;


implementation

constructor TTagRepository.Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction);
begin
  inherited Create;
  FSQLConnection:= AConnection;
  FSQLTransaction:= ATransaction;
end;

procedure TTagRepository.Delete(const AId: Integer);
var
  SQLQuery: TSQLQuery;
begin
  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;
    SQLQuery.SQL.Text:= 'DELETE FROM tags WHERE id = ' + IntToStr(AId);
    SQLQuery.ExecSQL;
    FSQLTransaction.Commit;
    SQLQuery.Close;
  finally
    SQLQuery.Free;
  end;
end;

procedure TTagRepository.Update(const AId: Integer; const ATag: string);
var
  SQLQuery: TSQLQuery;
begin
  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;
    SQLQuery.SQL.Text:= 'UPDATE tags SET label = ' + QuotedStr(ATag) + ' WHERE id = ' + IntToStr(AId);
    SQLQuery.ExecSQL;
    FSQLTransaction.Commit;
    SQLQuery.Close;
  finally
    SQLQuery.Free;
  end;
end;

function TTagRepository.Add(const ATag: string): TTag;
var
  SQLQuery: TSQLQuery;
  Id: Integer;
begin
  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;
    SQLQuery.SQL.Text:= 'INSERT INTO tags (label) VALUES (:label)';
    SQLQuery.ParamByName('label').AsString:= ATag;
    SQLQuery.ExecSQL;
    FSQLTransaction.Commit;

    SQLQuery.SQL.Clear;
    SQLQuery.SQL.Text:= 'SELECT last_insert_rowid()';
    SQLQuery.Open;
    Id:= SQLQuery.Fields[0].AsInteger;
    SQLQuery.Close;

    Result:= TTag.Create(Id, ATag);
  finally
    SQLQuery.Free;
  end;
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

    SQLQuery.SQL.Text:= 'SELECT id, label FROM tags ORDER BY label;';
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

