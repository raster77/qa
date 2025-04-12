unit UDataModuleQa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB;

type

  { TDataModuleQa }

  TDataModuleQa = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FSQLite3Connection: TSQLite3Connection;
    FSQLTransaction: TSQLTransaction;
    DbScript: TStringList;
    Triggers: TStringList;
  public
    function Open(const ADatabase: string): boolean;
    function CreateDb(const ADatabase: string): boolean;
    property DBConn: TSQLite3Connection read FSQLite3Connection;
    property DefaultTransaction: TSQLTransaction read FSQLTransaction;
  end;

var
  DataModuleQa: TDataModuleQa;

implementation

{$R *.lfm}

{ TDataModuleQa }

function TDataModuleQa.Open(const ADatabase: string): boolean;
begin
  Result := False;
  if FileExists(ADatabase) then
  begin
    try
      FSQLite3Connection.DatabaseName := ADatabase;
      FSQLite3Connection.Open;
      Result := FSQLite3Connection.Connected;
    except
      on E: Exception do
        Result := False;
    end;
  end;
end;

function TDataModuleQa.CreateDb(const ADatabase: string): boolean;
var
  SqlScript: TSQLScript;
  SqlQuery: TSQLQuery;
  SQLTransaction: TSQLTransaction;
  i: integer;
begin
  Result:= False;

  if not FileExists(ADatabase) then
  begin
    try
      try
        FSQLite3Connection.CloseTransactions;
        FSQLite3Connection.Close(True);
        FSQLite3Connection.DatabaseName:= ADatabase;

        SQLTransaction:= TSQLTransaction.Create(nil);
        FSQLite3Connection.Transaction:= SQLTransaction;
        SQLTransaction.Database:= FSQLite3Connection;
        FSQLite3Connection.Open;

        SqlScript:= TSQLScript.Create(nil);
        SqlQuery:= TSQLQuery.Create(nil);

        SqlScript.DataBase := FSQLite3Connection;
        SqlScript.Transaction := FSQLite3Connection.Transaction;
        SqlScript.Script.Assign(DbScript);
        SqlScript.Execute;
        SQLTransaction.Commit;

        SqlQuery.DataBase := FSQLite3Connection;
        SqlQuery.Transaction := FSQLite3Connection.Transaction;

        for i := 0 to Triggers.Count - 1 do
        begin
          SqlQuery.SQL.Clear;
          SqlQuery.SQL.Text := Triggers[i];
          SqlQuery.ExecSQL;
          SQLTransaction.Commit;
        end;
        SqlQuery.Close;

        FSQLite3Connection.CloseTransactions;
        FSQLite3Connection.Close(True);
        Open(ADatabase);
        Result:= True;
      except
        on E: Exception do
          Result := False;
      end;
    finally
      SqlScript.Free;
      SqlQuery.Free;
      SQLTransaction.Free;
    end;
  end;
end;

procedure TDataModuleQa.DataModuleCreate(Sender: TObject);
begin
  FSQLite3Connection := TSQLite3Connection.Create(nil);
  FSQLTransaction := TSQLTransaction.Create(nil);

  FSQLite3Connection.Transaction := FSQLTransaction;
  DbScript := TStringList.Create;
  Triggers := TStringList.Create;
  DbScript.Text := 'CREATE TABLE IF NOT EXISTS questions ( ' +
    '	id	INTEGER NOT NULL, ' +
    '	question	TEXT NOT NULL, ' +
    '	PRIMARY KEY(id AUTOINCREMENT) ' + '); ' +
    'CREATE TABLE IF NOT EXISTS answers ( ' +
    '	id	INTEGER NOT NULL, ' +
    '	id_question	INTEGER NOT NULL, ' +
    '	answer	TEXT NOT NULL, ' +
    '	PRIMARY KEY(id AUTOINCREMENT), ' +
    '	FOREIGN KEY(id_question) REFERENCES questions(id) ' +
    '); ' + 'CREATE TABLE IF NOT EXISTS tags ( ' +
    '	id	INTEGER NOT NULL, ' +
    '	label	TEXT NOT NULL, ' +
    '	PRIMARY KEY(id AUTOINCREMENT) ' + '); ' +
    'CREATE TABLE IF NOT EXISTS tags_questions ( ' +
    '	id_question	INTEGER NOT NULL, ' +
    '	id_tag	INTEGER NOT NULL, ' +
    '	CONSTRAINT idx_unique_tag_question UNIQUE(id_question,id_tag), ' +
    '	FOREIGN KEY(id_question) REFERENCES questions(id), ' +
    '	FOREIGN KEY(id_tag) REFERENCES tags(id) ' +
    '); ' +
    'CREATE VIRTUAL TABLE IF NOT EXISTS questions_fts USING fts5(id, question, content=''questions'', content_rowid=''id''); ';

  Triggers.Add('CREATE TRIGGER questions_trg_delete AFTER DELETE ON questions BEGIN '
             + '  INSERT INTO questions_fts(id, question) VALUES(''delete'', old.id); '
             + 'END; ');
  Triggers.Add('CREATE TRIGGER questions_trg_delete_before BEFORE DELETE ON questions BEGIN '
             + '  DELETE FROM answers where id_question = old.id; '
             + '  DELETE FROM tags_questions WHERE id_question = old.id; '
             + 'END; ');
  Triggers.Add('CREATE TRIGGER questions_trg_insert AFTER INSERT ON questions BEGIN '
             + '  INSERT INTO questions_fts(rowid, question) VALUES (new.id, new.question); '
             + 'END; ');
  Triggers.Add('CREATE TRIGGER questions_trg_update AFTER UPDATE ON questions BEGIN '
             + '  INSERT INTO questions_fts(id, question) VALUES(''delete'', old.id); '
             + '  INSERT INTO questions_fts(rowid, question) VALUES (new.id, new.question); '
             + 'END; ');
end;

procedure TDataModuleQa.DataModuleDestroy(Sender: TObject);
begin
  DbScript.Free;
  Triggers.Free;
  FSQLite3Connection.CloseTransactions;
  FSQLTransaction.Free;
  FSQLite3Connection.Close(True);
  FSQLite3Connection.Free;
end;

end.
