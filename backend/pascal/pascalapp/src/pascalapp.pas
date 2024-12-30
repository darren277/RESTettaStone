program main;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, fphttpapp, httpdefs, httproute, fpjson, jsonparser,
  Dos,
  Classes, server, users;

var
  rootPath: string;
  portStr: string;
  port: integer;
  code: integer;
  hnd: TUsersAppHandler;
begin
  rootPath := ExtractFilePath(ParamStr(0));
  portStr := GetEnv('PORT');
  Val(portStr, port, code);

  hnd := TUsersAppHandler.Create(rootPath);
  try
    Application.Port := port;

    HTTPRouter.RegisterRoute('/users/:id', @hnd.usersApi);

    // static and redirect to index
    HTTPRouter.RegisterRoute('/*', @hnd.static);
    HTTPRouter.RegisterRoute('/', @hnd.indexDir, true);

    WriteLn('Server started on port ', port);

    Application.OnException := @hnd.OnErr;
    Application.Threaded := true;
    Application.Initialize;
    Application.Run;

  finally
    hnd.Free;
  end;
end.
