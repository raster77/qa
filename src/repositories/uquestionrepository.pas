unit UQuestionRepository;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, UQuestion, fgl, UTag;

Type

  TIntegerList = specialize TFPGList<Integer>;

  TQuestionRepository = class
  private
    FSQLConnection: TSQLConnection;
    FSQLTransaction: TSQLTransaction;
  public
    constructor Create(const AConnection: TSQLConnection;
                       const ATransaction: TSQLTransaction);
    function InList(const AQuestionList: TQuestionList; const AId: Integer): boolean;
    function FtsSearch(const AQuestion: string; const Tags: TIntegerList): TQuestionList;
    function Add(const AQuestion: string): Integer;
    procedure Update(const AId: Integer; const AQuestion: string);
    function GetTags(const AId: Integer): TTagList;
    procedure SetTags(const AId: Integer; const ATagsId: TIntegerList);
    procedure DeleteTags(const AId: Integer; const ATagsId: TIntegerList);
    function Load(AId: Integer): TQuestion;
    function Count: Integer;
  end;

implementation

function TQuestionRepository.GetTags(const AId: Integer): TTagList;
var
  SQLQuery: TSQLQuery;
begin
  Result:= TTagList.Create;
  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;
    SQLQuery.SQL.Add('SELECT t.id, t.label FROM questions q ');
    SQLQuery.SQL.Add('LEFT JOIN tags_questions tq ON tq.id_question = q.id ');
    SQLQuery.SQL.Add('LEFT JOIN tags t ON t.id = tq.id_tag ');
    SQLQuery.SQL.Add('WHERE q.id = :qId');
    SQLQuery.ParamByName('qId').AsInteger:= AId;
    SQLQuery.Open;
    While not SQLQuery.EOF do
    begin
      if (not SQLQuery.FieldByName('id').IsNull) and (not SQLQuery.FieldByName('label').IsNull) then
        Result.Add(TTag.Create(SQLQuery.FieldByName('id').AsInteger,
                               SQLQuery.FieldByName('label').AsString));
      SQLQuery.Next;
    end;
    SQLQuery.Close;
  finally
    SQLQuery.Free;
  end;

end;

procedure TQuestionRepository.DeleteTags(const AId: Integer; const ATagsId: TIntegerList);
var
  i: Integer;
  SQLQuery: TSQLQuery;
  Exists: Boolean;
begin
  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;

    for i:= 0 to ATagsId.Count -1 do
    begin
      SQLQuery.SQL.Clear;
      SQLQuery.SQL.Add('SELECT COUNT(*) FROM tags_questions ');
      SQLQuery.SQL.Add('WHERE id_question = ' + IntToStr(AId) + ' AND id_tag = ' + IntToStr(ATagsId[i]));
      SQLQuery.Open;
      Exists:= SQLQuery.Fields[0].AsInteger > 0;
      SQLQuery.Close;

      if Exists then
      begin
        SQLQuery.SQL.Clear;
        SQLQuery.SQL.Add('DELETE FROM tags_questions WHERE id_question = ' + IntToStr(AId) + ' ');
        SQLQuery.SQL.Add('and id_tag = ' + IntToStr(ATagsId[i]));
        SQLQuery.ExecSQL;
      end;
      FSQLTransaction.Commit;
    end;

  finally
      SQLQuery.Free;
  end;
end;

procedure TQuestionRepository.SetTags(const AId: Integer; const ATagsId: TIntegerList);
var
  i: Integer;
  SQLQuery: TSQLQuery;
  Exists: Boolean;
begin
  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;

    for i:= 0 to ATagsId.Count -1 do
    begin
      SQLQuery.SQL.Clear;
      SQLQuery.SQL.Add('SELECT COUNT(*) FROM tags_questions ');
      SQLQuery.SQL.Add('WHERE id_question = ' + IntToStr(AId) + ' AND id_tag = ' + IntToStr(ATagsId[i]));
      SQLQuery.Open;
      Exists:= SQLQuery.Fields[0].AsInteger > 0;
      SQLQuery.Close;
      if not Exists then
      begin
        SQLQuery.SQL.Clear;
        SQLQuery.SQL.Text:= 'INSERT INTO tags_questions (id_question, id_tag) VALUES(:idQ, :idT)';
        SQLQuery.ParamByName('idQ').AsInteger:= AId;
        SQLQuery.ParamByName('idT').AsInteger:= ATagsId[i];
        SQLQuery.ExecSQL;
      end;
      FSQLTransaction.Commit;
    end;

  finally
      SQLQuery.Free;
  end;
end;

function TQuestionRepository.Count: Integer;
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
      Text:= 'SELECT count(id) as total FROM questions';
    end;

    SQLQuery.Open;
    Result:= SQLQuery.FieldByName('total').AsInteger;
  finally
    SQLQuery.Free;
  end;
end;

function TQuestionRepository.FtsSearch(const AQuestion: string; const Tags: TIntegerList): TQuestionList;
var
  SQLQuery: TSQLQuery;
  SqlQuestionFts: String;
  SqlQuestionTags: String;
  SqlTags: String;
  i: Integer;
begin
  Result:= TQuestionList.Create;
  SqlQuestionFts:= '';
  SqlQuestionTags:= '';
  SqlTags:= '';

  SQLQuery:= TSQLQuery.Create(nil);
  try
    SQLQuery.SQLConnection:= FSQLConnection;
    SQLQuery.Transaction:= FSQLTransaction;

    if Trim(AQuestion) <> '' then
    begin
      SqlQuestionFts:= 'SELECT id, question FROM questions_fts q ';
      SqlQuestionFts:= SqlQuestionFts + 'WHERE question MATCH ' + QuotedStr(Trim(AQuestion)) + ' ';
      SqlQuestionFts:= SqlQuestionFts + 'ORDER BY bm25(questions_fts)';
      SQLQuery.SQL.Text:= SqlQuestionFts;
      SQLQuery.Open;
      while not SQLQuery.EOF do
      begin
        Result.Add(TQuestion.Create(SQLQuery.FieldByName('id').AsInteger,
                                    SQLQuery.FieldByName('question').AsString));
        SQLQuery.Next;
      end;
      SQLQuery.Close;
    end;

    if Tags.Count > 0 then
    begin
      SqlQuestionTags:= 'SELECT id, question FROM questions_fts q ';
      SqlQuestionTags:= SqlQuestionTags + 'JOIN tags_questions t on t.id_question = q.id ';
      SqlTags:= SqlTags + 'WHERE id_tag in (';
      for i:= 0 to Tags.Count -1 do
      begin
        SqlTags:= SqlTags + IntToStr(tags[i]);
        if i < Tags.Count -1 then
          SqlTags:= SqlTags + ',';
      end;
      SqlTags:= SqlTags + ')';

      SQLQuery.SQL.Clear;
      SQLQuery.SQL.Text:= SqlQuestionTags + SqlTags;
      SQLQuery.Open;
      while not SQLQuery.EOF do
      begin

        if not InList(Result, SQLQuery.FieldByName('id').AsInteger) then
          Result.Add(TQuestion.Create(SQLQuery.FieldByName('id').AsInteger,
                                      SQLQuery.FieldByName('question').AsString));

        SQLQuery.Next;
      end;
      SQLQuery.Close;
    end;
  finally
    SQLQuery.Free;
  end;
end;

function TQuestionRepository.InList(const AQuestionList: TQuestionList; const AId: Integer): boolean;
var
  i: Integer;
begin
  Result:= false;
  for i:= 0 to AQuestionList.Count -1 do
  begin
    if Aid = AQuestionList.Items[i].Id then
    begin
      Result:= True;
      Exit;
    end;
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

