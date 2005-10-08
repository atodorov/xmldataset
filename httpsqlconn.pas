unit httpsqlconn;

{$mode objfpc}{$H+}

{*******************************************************************************

 This file is part of the XMLDataset suite for the Free Component Library!

 (c) 2005 by Alexander Todorov.
 e-mail: alexx.todorov@gmail.com

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING included in this distribution,                      *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
*******************************************************************************}

//{$DEFINE DEBUGCONNECTION}

interface

uses Classes, SysUtils, SQLConnection
     {$IFDEF WIN32} //todo : change this to only HTTPClient
     ,HTTPSend in 'synapse/source/lib/httpsend.pas'
     {$ELSE}
       {$IFDEF LINUX}
       ,HTTPClient
       {$ENDIF}
     {$ENDIF}
     ;

const
  HTTP_USER_AGENT = 'user-agent';
  HTTP_URL = 'url';
  HTTP_PROTOCOL = 'protocol'; // http protocol version 0.9, 1.0, 1.1 (default)
  HTTP_PROTOCOL_0_9 = '0.9';
  HTTP_PROTOCOL_1_0 = '1.0';
  HTTP_PROTOCOL_1_1 = '1.1';
  HTTP_METHOD = 'method'; // allowed values are POST, GET
  HTTP_METHOD_POST = 'POST';
  HTTP_METHOD_GET  = 'GET';
  HTTP_COOKIES  = 'cookies';
  HTTP_MIMETYPE = 'mimetype'; // is it needed ???

  PROXY_HOST = 'proxy_host';
  PROXY_PORT = 'proxy_port';
  PROXY_USER = 'proxy_user';
  PROXY_PASS = 'proxy_pass';

type

  { THTTPSQLConnection }
  THTTPSQLConnection = class(TBaseSQLConnection)
  private
    FHttpClient : {$IFDEF WIN32} THTTPSend;   {$ENDIF}
                  {$IFDEF LINUX} THTTPClient; {$ENDIF}
    FHttpURL : String;
    procedure DoSetConnectionParams;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function  GetConnected: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;  override;
    procedure Close; override;
    function  HttpPostFile(const FieldName, FileName: string;
                           const Data, ResultData : TStream): Boolean;
    //todo : remove if not needed
//    property Cookies : TStringList // http connection cookies - session management
  end;
  
implementation

{$IFDEF DEBUGCONNECTION}
uses Dialogs;
{$ENDIF}


(*******************************************************************************
{ THTTPSQLConnection }
*******************************************************************************)

procedure THTTPSQLConnection.DoSetConnectionParams;
var index : Integer;
begin
// HTTP_URL
  index := ConnParams.IndexOfName(HTTP_URL);
  if (index > -1)
    then FHttpUrl := ConnParams.ValueFromIndex[index]
    else raise Exception.Create('Required parameter HTTP_URL is missing!');

// HTTP_PROTOCOL
  index := ConnParams.IndexOfName(HTTP_PROTOCOL);
  if (index > -1) then FHttpClient.Protocol := ConnParams.ValueFromIndex[index];

// USER AGENT
  index := ConnParams.IndexOfName(HTTP_USER_AGENT);
  if (index > -1) then FHttpClient.UserAgent := ConnParams.ValueFromIndex[index];

// PROXY_HOST
  index := ConnParams.IndexOfName(PROXY_HOST);
  if (index > -1) then FHttpClient.ProxyHost := ConnParams.ValueFromIndex[index];

// PROXY_PORT
  index := ConnParams.IndexOfName(PROXY_PORT);
  if (index > -1) then FHttpClient.ProxyPort := ConnParams.ValueFromIndex[index];

// PROXY_USER
  index := ConnParams.IndexOfName(PROXY_USER);
  if (index > -1) then FHttpClient.ProxyUser := ConnParams.ValueFromIndex[index];

// PROXY_PASS
  index := ConnParams.IndexOfName(PROXY_PASS);
  if (index > -1) then FHttpClient.ProxyPass := ConnParams.ValueFromIndex[index];
end;

procedure THTTPSQLConnection.DoConnect;
begin
  DoSetConnectionParams;
// do nothing. no persistent connections here.
end;

procedure THTTPSQLConnection.DoDisconnect;
begin // todo
// do nothing. no persistent connections here.
// clear session cookies maybe
end;

function THTTPSQLConnection.GetConnected: Boolean;
begin
// no persistent connections here.
  Result := false;
end;

constructor THTTPSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF WIN32}
     FHttpClient := THTTPSend.Create;
  {$ENDIF}
  {$IFDEF LINUX}
     FHttpClient := THTTPClient.Create;
  {$ENDIF}
  FHttpClient.Protocol := HTTP_PROTOCOL_1_1;
  FHttpURL := '';
end;

destructor THTTPSQLConnection.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

procedure THTTPSQLConnection.Open;  // todo : fix
begin
//  FHttpClient.;
  inherited Open;
end;

procedure THTTPSQLConnection.Close; // todo : fix
begin
//  FHttpClient.;
  inherited Close;
end;

function THTTPSQLConnection.HttpPostFile(const FieldName, FileName: String;
                                         const Data, ResultData : TStream): Boolean;
const
  CRLF = #$0D + #$0A;
var
  Bound, S : String;
begin
  DoSetConnectionParams;
  
  Bound := IntToHex(Random(MaxInt), 8) + '_HTTPSQLConnection_Boundary';
  S := '--' + Bound + CRLF;
  S := S + 'content-disposition: form-data; name="' + FieldName + '";';
  S := S + ' filename="' + FileName +'"' + CRLF;
  S := S + 'Content-Type: Application/octet-string' + CRLF + CRLF;
  FHttpClient.Document.Clear;
  FHttpClient.Document.Write(Pointer(S)^, Length(S));
  FHttpClient.Document.CopyFrom(Data, 0);
  S := CRLF + '--' + Bound + '--' + CRLF;
  FHttpClient.Document.Write(Pointer(S)^, Length(S));
  FHttpClient.MimeType := 'multipart/form-data, boundary=' + Bound;
  Result := FHttpClient.HTTPMethod(HTTP_METHOD_POST, FHttpURL);
  ResultData.CopyFrom(FHttpClient.Document,0);
end;

end.

