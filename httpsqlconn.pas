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

{$DEFINE DEBUGCONNECTION}

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
  // xml processor parameters constants
  SQL_CONNECTION_STRING = 'connstr';

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
  public
    FHttpClient : {$IFDEF WIN32} THTTPSend; {$ELSE} {$IFDEF LINUX} THTTPClient; {$ENDIF} {$ENDIF}
  private
    FHttpURL : String;
    FHttpMethod : String;
    function  GetDocument: TMemoryStream;
    procedure SetDocument(const AValue: TMemoryStream);
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
    function  HttpPostFile(const URL, FieldName, FileName: string;
                           const Data: TStream;
                           ResultData: TStrings): Boolean;
    // XML document that is being sent / received
    property Document: TMemoryStream read GetDocument write SetDocument;
//    property Cookies : TStringList // http connection cookies - session management
  end;
  
implementation

{$IFDEF DEBUGCONNECTION}
uses Dialogs;
{$ENDIF}


(*******************************************************************************
{ THTTPSQLConnection }
*******************************************************************************)

function THTTPSQLConnection.GetDocument: TMemoryStream;
begin
  Result := FHttpClient.Document;
end;

procedure THTTPSQLConnection.SetDocument(const AValue: TMemoryStream);
begin
  FHttpClient.Document.CopyFrom(AValue,0); // copy entire document
end;

procedure THTTPSQLConnection.DoSetConnectionParams;
var index : Integer;
begin
// HTTP_URL
  index := ConnParams.IndexOfName(HTTP_URL);
  if (index > -1)
    then FHttpUrl := ConnParams.ValueFromIndex[index]
    else raise Exception.Create('Required parameter HTTP_URL is missing!');

// HTTP_METHOD
  index := ConnParams.IndexOfName(HTTP_METHOD);
  if (index > -1) then FHttpMethod := ConnParams.ValueFromIndex[index];

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
  FHttpClient.HTTPMethod(FHttpMethod,FHttpURL);
end;

procedure THTTPSQLConnection.DoDisconnect;
begin // todo
// do nothing. no persistent connections here.
// clear session cookies maybe
end;

function THTTPSQLConnection.GetConnected: Boolean;
begin
  Result := false;
end;

constructor THTTPSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF WIN32}
     FHttpClient := THTTPSend.Create;
  {$ELSE}
     {$IFDEF LINUX}
        FHttpClient := THTTPClient.Create;
     {$ENDIF}
  {$ENDIF}
  FHttpClient.Protocol := HTTP_PROTOCOL_1_1;

  FHttpMethod := HTTP_METHOD_POST;
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

function THTTPSQLConnection.HttpPostFile(const URL, FieldName, FileName: string;
                                         const Data: TStream;
                                         ResultData: TStrings): Boolean;
begin
  httpsend.HttpPostFile(URL,FieldName,FileName,Data,ResultData);
{$IFDEF DEBUGCONNECTION}
  ShowMessage(ResultData.Text);
{$ENDIF}
//  httpsend.HttpPostBinary(URL,Data); ResultData.LoadFromStream(Data);
end;

end.

