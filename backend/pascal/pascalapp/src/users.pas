unit users;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, httproute, fpjson, jsonparser,
  sqldb, ibconnection, pqconnection,
  Dos,
  server;

type
  { TUsersAppHandler }
  TUsersAppHandler = class(TAppHandler)
  private
    FConnection: TPQConnection;
    FTransaction: TSQLTransaction;
    function LoadData(): TJSONArray;
    function RecordExists(table, field, value: string): boolean;
    procedure DeleteRecord(table, field, value: string);
    procedure UpdateRecord(table, idField, idValue: string; data: TJSONObject);
    procedure SaveData(AData: TJSONObject);
    procedure InitializeConnection;
  public
    constructor Create(ARootPath: string);
    destructor Destroy; override;

    procedure usersApi(req: TRequest; res: TResponse);
  end;

implementation


{ TUsersAppHandler }
constructor TUsersAppHandler.Create(ARootPath: string);
begin
  inherited Create(ARootPath);
  FConnection := TPQConnection.Create(nil);
  FTransaction := TSQLTransaction.Create(FConnection);

  FConnection.Transaction := FTransaction;
  FConnection.UserName := GetEnv('PG_USER');
  FConnection.Password := GetEnv('PG_PASS');
  FConnection.DatabaseName := GetEnv('PG_DB');
  FConnection.HostName := GetEnv('PG_HOST');
  FConnection.Params.Add('port=' + GetEnv('PG_PORT'));
end;

destructor TUsersAppHandler.Destroy;
begin
  if FConnection.Connected then
    FConnection.Close;

  FTransaction.Free;
  FConnection.Free;

  inherited Destroy;
end;

procedure TUsersAppHandler.InitializeConnection;
begin
  FConnection.UserName := GetEnv('PG_USER');
  FConnection.Password := GetEnv('PG_PASS');
  FConnection.DatabaseName := GetEnv('PG_DB');
  FConnection.HostName := GetEnv('PG_HOST');
  FConnection.Params.Add('port=' + GetEnv('PG_PORT'));
end;

function TUsersAppHandler.LoadData(): TJSONArray;
var
  C: TPQConnection;
  T: TSQLTransaction;
  Q: TSQLQuery;
  jusers: TJSONArray;
begin
  Result := nil;
  C := nil;
  T := nil;
  Q := nil;
  jusers := TJSONArray.Create;

  try
    C := TPQConnection.Create(nil);
    T := TSQLTransaction.Create(C);
    Q := TSQLQuery.Create(C);

    C.UserName := GetEnv('PG_USER');
    C.Password := GetEnv('PG_PASS');
    C.DatabaseName := GetEnv('PG_DB');
    C.HostName := GetEnv('PG_HOST');
    C.Params.Add('port=' + GetEnv('PG_PORT'));
    C.Transaction := T;

    try
      C.Connected := True;
    except
      on E: Exception do
        raise Exception.Create('Error connecting to database: ' + E.Message);
    end;

    Q.Database := C;
    Q.Transaction := T;
    Q.SQL.Text := 'SELECT id, email FROM USERS';

    try
      Q.Open;
    except
      on E: Exception do
        raise Exception.Create('Error executing query: ' + E.Message);
    end;

    try
      while not Q.EOF do
      begin
        jusers.Add(TJSONObject.Create([
          'id', Q.FieldByName('id').AsString,
          'email', Q.FieldByName('email').AsString
        ]));
        Q.Next;
      end;
    except
      on E: Exception do
        raise Exception.Create('Error processing query results: ' + E.Message);
    end;

    Result := jusers;
  finally
    // Cleanup resources
    if Assigned(Q) then Q.Free;
    if Assigned(T) then T.Free;
    if Assigned(C) then
    begin
      if C.Connected then
        C.Connected := False;
      C.Free;
    end;

    // If something went wrong, ensure jusers is freed
    if not Assigned(Result) then
      jusers.Free;
  end;
end;

procedure TUsersAppHandler.SaveData(AData: TJSONObject);
var
  Q: TSQLQuery;
  emailField: string;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.Transaction := FConnection.Transaction;

    emailField := AData.Strings['email'];

    //Q.SQL.Text := 'INSERT INTO users (id, email) VALUES (:id, :email) ' +
    //              'ON CONFLICT (id) DO UPDATE SET email = :email;';
    //Q.Params.ParamByName('id').AsInteger := idField;
    //Q.Params.ParamByName('email').AsString := emailField;

    Q.SQL.Text := 'INSERT INTO users (email) VALUES (:email)';
    Q.Params.ParamByName('email').AsString := emailField;

    try
      FConnection.Open;
      Q.ExecSQL;
      FConnection.Transaction.Commit;
    except
      on E: Exception do
      begin
        FConnection.Transaction.Rollback;
        raise Exception.Create('Error saving data to database: ' + E.Message);
      end;
    end;
  finally
    Q.Free;
    FConnection.Close;
  end;
end;

function TUsersAppHandler.RecordExists(table, field, value: string): boolean;
var
  Q: TSQLQuery;
begin
  Result := False; // Default to false
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;

    // Handle type consistency by checking the field
    if field = 'id' then
    begin
      Q.SQL.Text := Format('SELECT 1 FROM %s WHERE %s = :id LIMIT 1', [table, field]);
      Q.Params.ParamByName('id').AsInteger := StrToInt(value);
    end
    else
    begin
      Q.SQL.Text := Format('SELECT 1 FROM %s WHERE %s = :value LIMIT 1', [table, field]);
      Q.Params.ParamByName('value').AsString := value;
    end;

    try
      Q.Open;
      Result := not Q.EOF; // True if a record exists
    except
      on E: Exception do
      begin
        raise Exception.Create('Error in RecordExists: ' + E.Message);
      end;
    end;
  finally
    Q.Free;
  end;
end;

procedure TUsersAppHandler.DeleteRecord(table, field, value: string);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;

    Q.SQL.Text := Format('DELETE FROM %s WHERE %s = :id', [table, field]);
    Q.Params.ParamByName('id').AsInteger := StrToInt(value); // Convert "value" to integer

    try
      Q.ExecSQL;
      FTransaction.Commit;
    except
      on E: Exception do
      begin
        FTransaction.Rollback;
        raise Exception.Create('Error deleting record: ' + E.Message);
      end;
    end;
  finally
    Q.Free;
  end;
end;

procedure TUsersAppHandler.UpdateRecord(table, idField, idValue: string; data: TJSONObject);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.SQL.Text := Format(
      'UPDATE %s SET email = :email WHERE %s = :id',
      [table, idField]
    );
    Q.Params.ParamByName('email').AsString := data.Strings['email'];
    Q.Params.ParamByName('id').AsInteger := StrToInt(idValue);

    try
      Q.ExecSQL;
      FTransaction.Commit;
    except
      on E: Exception do
      begin
        FTransaction.Rollback;
        raise Exception.Create('Error updating record: ' + E.Message);
      end;
    end;
  finally
    Q.Free;
  end;
end;

procedure TUsersAppHandler.usersApi(req: TRequest; res: TResponse);
var
  id: string;
  jparam, singleUser: TJSONObject;
  jres: TJSONArray;
  i: Integer;
  found: boolean;
begin
  if CompareText(req.Method, 'GET') = 0 then
  begin
    try
      jres := LoadData();

      // Check if an 'id' is provided
      id := req.RouteParams['id']; // Correct usage of RouteParams

      if id <> '' then
      begin
        found := False;
        for i := 0 to jres.Count - 1 do
        begin
          singleUser := TJSONObject(jres.Items[i]);
          if singleUser.Integers['id'] = StrToInt(id) then
          begin
            found := True;
            jsonResponse(res, singleUser);
            Break;
          end;
        end;

        if not found then
        begin
          res.Code := 404;
          res.Content := '{"error":"User not found"}';
          res.SendContent;
        end;
      end
      else
      begin
        // Return all users
        jsonResponse(res, jres);
      end;
    except
      on E: Exception do
      begin
        writeln('Error: ' + E.Message);
        res.Code := 500;
        res.Content := '{"error":"Error loading data"}';
        res.SendContent;
      end;
    end;
  end
  else if CompareText(req.Method, 'POST') = 0 then
  begin
    try
      jparam := TJSONObject(GetJSON(req.Content, False));
      try
        SaveData(jparam);
        res.Code := 200;
        jsonResponse(res, TJSONObject.Create(['message', 'User created']));
      finally
        jparam.Free;
      end;
    except
      on E: Exception do
      begin
        writeln('Error: ' + E.Message);
        res.Code := 500;
        res.Content := '{"error":"Error creating user"}';
        res.SendContent;
      end;
    end;
  end
  else if CompareText(req.Method, 'PUT') = 0 then
  begin
    id := req.RouteParams['id'];
    try
      if not RecordExists('users', 'id', id) then
        begin
          res.Code := 404;
          res.Content := '{"error":"User not found"}';
          res.SendContent;
          Exit;
        end;
      jparam := TJSONObject(GetJSON(req.Content, False));
      try
        UpdateRecord('users', 'id', id, jparam);
        jsonResponse(res, TJSONObject.Create(['message', 'User updated']));
      finally
        jparam.Free;
      end;
    except
      on E: Exception do
      begin
        writeln('Error updating user: ' + E.Message);
        res.Code := 500;
        res.Content := '{"error":"Error updating user"}';
        res.SendContent;
      end;
    end;
  end
  else if CompareText(req.Method, 'DELETE') = 0 then
  begin
    id := req.RouteParams['id'];
    try
      if not RecordExists('users', 'id', id) then
      begin
        res.Code := 404;
        res.Content := '{"error":"User not found"}';
        res.SendContent;
        Exit;
      end;

      DeleteRecord('users', 'id', id);
      jsonResponse(res, TJSONObject.Create(['message', 'User deleted']));
    except
      on E: Exception do
      begin
        writeln('Error: ' + E.Message);
        res.Code := 500;
        res.Content := '{"error":"Error deleting user"}';
        res.SendContent;
      end;
    end;
  end
  else
  begin
    res.Code := 405;
    res.Content := '{"error":"Method not allowed"}';
    res.SendContent;
  end;
end;

end.
