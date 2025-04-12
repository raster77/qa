unit UQuestionRepository;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, UQuestion;

Type

  TQuestionRepository = class
  private
    FSQLConnection: TSQLConnection;
    FSQLTransaction: TSQLTransaction;
  public
    constructor Create(const AConnection: TSQLConnection;
                       const ATransaction: TSQLTransaction);
    function FtsSearch(const AQuestion: string): TQuestionList;
    function Add(const AQuestion: string): Integer;
    procedure Update(const AId: Integer; const AQuestion: string);
    function Load(AId: Integer): TQuestion;
  end;

implementation

function TQuestionRepository.FtsSearch(const AQuestion: string): TQuestionList;
var
  SQLQuery: TSQLQuery;
begin
  Result:= TQuestionList.Create;

  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;

    with SQLQuery.SQL do
    begin
      Text:= 'SELECT id, question FROM questions_fts ';
      Text:= Text + 'WHERE question MATCH ' + QuotedStr(Trim(AQuestion)) + ' ';
      Text:= Text + 'ORDER BY bm25(questions_fts)';
    end;

    SQLQuery.Open;

    while not SQLQuery.EOF do
    begin
      Result.Add(TQuestion.Create(SQLQuery.FieldByName('id').AsInteger,
                                  SQLQuery.FieldByName('question').AsString));
      SQLQuery.Next;
    end;
  finally
    SQLQuery.Free;
  end;
end;

function TQuestionRepository.Load(AId: Integer): TQuestion;
var
  SQLQuery: TSQLQuery;
begin
  try
    SQLQuery:= TSQLQuery.Create(nil);
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;

    with SQLQuery.SQL do
    begin
      Text:= 'SELECT id, question FROM questions ';
      Text:= Text + 'WHERE id = ' + IntToStr(AId);
    end;

    SQLQuery.Open;
    Result:= TQuestion.Create(SQLQuery.FieldByName('id').AsInteger, SQLQuery.FieldByName('question').AsString);

  finally
      SQLQuery.Close;
      SQLQuery.Free;
  end;
end;

function TQuestionRepository.Add(const AQuestion: string): Integer;
var
  SQLQuery: TSQLQuery;
begin
  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;

    SQLQuery.SQL.Text:= 'INSERT INTO questions (question) VALUES (:q)';
    SQLQuery.Params.ParamByName('q').AsString:= AQuestion;
    SQLQuery.ExecSQL;
    FSQLTransaction.Commit;

    SQLQuery.SQL.Clear;
    SQLQuery.SQL.Text:= 'SELECT last_insert_rowid()';
    SQLQuery.Open;
    result:= SQLQuery.Fields[0].AsInteger;

    finally
      SQLQuery.Close;
      SQLQuery.Free;
    end;
end;

procedure TQuestionRepository.Update(const AId: Integer; const AQuestion: string);
var
  SQLQuery: TSQLQuery;
begin
  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;

    SQLQuery.SQL.Add('Update questions set question = :q');
    SQLQuery.SQL.Add('WHERE id = :id');
    SQLQuery.Params.ParamByName('id').AsInteger:= AId;
    SQLQuery.Params.ParamByName('q').AsString:= AQuestion;
    SQLQuery.ExecSQL;
    FSQLTransaction.Commit;
  finally
      SQLQuery.Close;
      SQLQuery.Free;
    end;
end;

constructor TQuestionRepository.Create(const AConnection: TSQLConnection;
                                       const ATransaction: TSQLTransaction);
begin
  inherited Create;
  FSQLConnection:= AConnection;
  FSQLTransaction:= ATransaction;
end;

end.

