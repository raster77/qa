unit UAnswerRepository;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, UAnswer;

Type

  TAnswerRepository = class
  private
    FSQLConnection: TSQLConnection;
    FSQLTransaction: TSQLTransaction;
    const ID_FIELD = 'id';
    const ANSWER_FIELD = 'answer';
    const IDQUESTION_FIELD = 'id_question';
  public
    constructor Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction);
    procedure Add(const AIdQuestion: Integer; const AAnswer: string);
    procedure Update(const AId: Integer; const AAnswer: string);
    function GetAnswerByQuestionId(const AQuestionId: Integer): TAnswer;
    function Count: Integer;
  end;

implementation

constructor TAnswerRepository.Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction);
begin
  FSQLConnection:= AConnection;
  FSQLTransaction:= ATransaction;
end;

function TAnswerRepository.Count: Integer;
var
  SQLQuery: TSQLQuery;
begin
  Result:= -1;
  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;

    with SQLQuery.SQL do
    begin
      Text:= 'SELECT count(id) as total FROM answers';
    end;

    SQLQuery.Open;
    Result:= SQLQuery.FieldByName('total').AsInteger;
  finally
    SQLQuery.Free;
  end;
end;

procedure TAnswerRepository.Add(const AIdQuestion: Integer; const AAnswer: string);
var
  SQLQuery: TSQLQuery;
begin
  try
    Try
      SQLQuery:= TSQLQuery.Create(nil);
      SQLQuery.SQLConnection:= FSQLConnection;
      SQLQuery.Transaction:= FSQLTransaction;
      SQLQuery.SQL.Text:= 'INSERT INTO answers (id_question, answer) VALUES (:idq, :answer)';
      SQLQuery.ParamByName('idq').AsInteger:= AIdQuestion;
      SQLQuery.ParamByName('answer').AsString:= Trim(AAnswer);
      SQLQuery.ExecSQL;
      FSQLTransaction.Commit;
    Except
      on E:Exception do
      // error
    end;
  finally
    SQLQuery.Free;
  end;
end;

procedure TAnswerRepository.Update(const AId: Integer; const AAnswer: string);
var
  SQLQuery: TSQLQuery;
begin
  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;

    SQLQuery.SQL.Add('Update answers set answer = :a');
    SQLQuery.SQL.Add('WHERE id = :id');
    SQLQuery.Params.ParamByName('id').AsInteger:= AId;
    SQLQuery.Params.ParamByName('a').AsString:= AAnswer;
    SQLQuery.ExecSQL;
    FSQLTransaction.Commit;
  finally
      SQLQuery.Close;
      SQLQuery.Free;
    end;
end;

function TAnswerRepository.GetAnswerByQuestionId(const AQuestionId: Integer): TAnswer;
var
  SQLQuery: TSQLQuery;
begin
  try
    Try
      SQLQuery:= TSQLQuery.Create(nil);
      SQLQuery.SQLConnection:= FSQLConnection;
      SQLQuery.Transaction:= FSQLTransaction;

      with SQLQuery.SQL do
      begin
        Text:= 'SELECT ' + ID_FIELD + ', ' + ANSWER_FIELD + ' ';
        Text:= Text + 'FROM answers ';
        Text:= Text + 'WHERE ' + IDQUESTION_FIELD + '=' + IntToStr(AQuestionId);
      end;
      SQLQuery.Open;

      Result:= TAnswer.Create(SQLQuery.FieldByName(ID_FIELD).AsInteger,
                              SQLQuery.FieldByName(ANSWER_FIELD).AsUTF8String);

    Except
      on E:Exception do
        Result.Free;
    end;
  finally
    SQLQuery.Free;
  end;
end;

end.

