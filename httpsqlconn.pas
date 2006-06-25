unit httpsqlconn;

{$mode objfpc}{$H+}
{$I xmldsdefs.inc}

{*******************************************************************************

 This file is part of the XMLDataset suite for the Free Component Library!
 Connection class that sends data over HTTP.

 (c) 2005-2006 by Alexander Todorov.
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

interface

uses Classes, SysUtils, CustomSQLConn,
     {$IFDEF USE_SYNAPSE} //todo : change this to only HTTPClient
     HTTPSend in 'synapse/source/lib/httpsend.pas'
     {$ELSE}
     HTTPClient, HTTPBase
     {$ENDIF}
     ;

const

  HTTP_PROTOCOL = 'protocol';
  HTTP_PROTOCOL_0_9 = '0.9';
  HTTP_PROTOCOL_1_0 = '1.0';
  HTTP_PROTOCOL_1_1 = '1.1';
  HTTP_METHOD = 'method';
  HTTP_METHOD_POST = 'POST';
  HTTP_METHOD_GET  = 'GET';
  HTTP_POST_FILENAME  = 'filename';
  HTTP_POST_FIELDNAME = 'fieldname';
  
  {$IFDEF USE_SYNAPSE}
  HTTP_URL = 'url';
  HTTP_USER_AGENT = 'user-agent';
  HTTP_COOKIES  = 'cookies';
  HTTP_MIMETYPE = 'mimetype';

  PROXY_HOST = 'proxy_host';
  PROXY_PORT = 'proxy_port';
  PROXY_USER = 'proxy_user';
  PROXY_PASS = 'proxy_pass';
  {$ELSE}
  HTTP_URL = fieldLocation;
  HTTP_USER_AGENT = fieldUserAgent;
  HTTP_COOKIES  = fieldCookie;
  HTTP_MIMETYPE = fieldContentType;
  {$ENDIF}

type

  { THTTPSQLConnection }
  THTTPSQLConnection = class(TCustomSQLConnection)
  private
    FHttpClient : {$IFDEF USE_SYNAPSE} THTTPSend {$ELSE} THTTPClient {$ENDIF} ;
    FHttpURL   : String;
    FFieldName : String;
    FFileName  : String;
    procedure DoSetConnectionParams;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function  GetConnected: Boolean; override;
    function  HttpPostFile: Boolean;
    function Open : Boolean;  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //todo : remove if not needed
//    property Cookies : TStringList // http connection cookies - session management
  end;
  
implementation


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
    else raise Exception.Create('Required parameter '+HTTP_URL+' is missing!');

// HTTP_POST_FILENAME
  index := ConnParams.IndexOfName(HTTP_POST_FILENAME);
  if (index > -1)
    then FFileName := ConnParams.ValueFromIndex[index]
    else raise Exception.Create('Required parameter '+HTTP_POST_FILENAME+' is missing!');

// HTTP_POST_FIELDNAME
  index := ConnParams.IndexOfName(HTTP_POST_FIELDNAME);
  if (index > -1)
    then FFieldName := ConnParams.ValueFromIndex[index]
    else raise Exception.Create('Required parameter '+HTTP_POST_FIELDNAME+' is missing!');
    
// HTTP_PROTOCOL
  index := ConnParams.IndexOfName(HTTP_PROTOCOL);
  if (index > -1) then
    {$IFDEF USE_SYNAPSE}
    FHttpClient.Protocol := ConnParams.ValueFromIndex[index];
    {$ELSE}
    FHttpClient.HeaderToSend.HttpVersion := ConnParams.ValueFromIndex[index];
    {$ENDIF}

// USER AGENT
  index := ConnParams.IndexOfName(HTTP_USER_AGENT);
  if (index > -1) then
    {$IFDEF USE_SYNAPSE}
    FHttpClient.UserAgent := ConnParams.ValueFromIndex[index];
    {$ELSE}
    FHttpClient.HeaderToSend.UserAgent := ConnParams.ValueFromIndex[index];
    {$ENDIF}


  {$IFDEF USE_SYNAPSE}
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
  {$ENDIF}
end;

procedure THTTPSQLConnection.DoConnect;
begin
  DoSetConnectionParams;
// do nothing. no persistent connections here.
end;

procedure THTTPSQLConnection.DoDisconnect;
begin
// no persistent connections here.
// clear session cookies maybe
   DataToSend   := nil;  // clear reference
   ReceivedData := nil;
end;

function THTTPSQLConnection.GetConnected: Boolean;
begin
// no persistent connections here.
  Result := false;
end;

constructor THTTPSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {$IFDEF USE_SYNAPSE}
  FHttpClient := THTTPSend.Create;
  {$ELSE}
  FHttpClient := THTTPClient.Create(nil);
  {$ENDIF}

  {$IFDEF USE_SYNAPSE}
  FHttpClient.Protocol := HTTP_PROTOCOL_1_1;
  {$ELSE}
  FHttpClient.HeaderToSend := THttpHeader.Create;
  FHttpClient.HeaderToSend.HttpVersion := HTTP_PROTOCOL_1_1;
  FHttpClient.StreamToSend := TMemoryStream.Create;
  {$ENDIF}
  FHttpURL   := '';
  FFileName  := '';
  FFieldName := '';
end;

destructor THTTPSQLConnection.Destroy;
begin
  {$IFNDEF USE_SYNAPSE}
  if Assigned(FHttpClient.StreamToSend) then
     FHttpClient.StreamToSend.Free;

  if Assigned(FHttpClient.HeaderToSend) then
     FHttpClient.HeaderToSend.Free;
  {$ENDIF}
     
  FHttpClient.Free;
  inherited Destroy;
end;

function THTTPSQLConnection.Open : Boolean;
begin
  DoSetConnectionParams;
  inherited Open;
  Result := HttpPostFile;
end;

function THTTPSQLConnection.HttpPostFile: Boolean;
const CRLF = #$0D + #$0A;
var Bound, S : String;
begin
  DoSetConnectionParams;
  
  Bound := IntToHex(Random(MaxInt), 8) + '_HTTPSQLConnection_Boundary';
  S := '--' + Bound + CRLF;
  S := S + 'Content-disposition: form-data; name="' + FFieldName + '";';
  S := S + ' filename="' + FFileName +'"' + CRLF;
  S := S + 'Content-Type: text/xml' + CRLF + CRLF;

  {$IFDEF USE_SYNAPSE}
  FHttpClient.Document.Clear;
  FHttpClient.Document.Write(Pointer(S)^, Length(S));
  FHttpClient.Document.CopyFrom(DataToSend, 0);
  {$ELSE}
  FHttpClient.StreamToSend.Position := 0; // try to clear contents
  FHttpClient.StreamToSend.Size := 0;
  FHttpClient.StreamToSend.Write(Pointer(S)^, Length(S));
  FHttpClient.StreamToSend.CopyFrom(DataToSend, 0);
  {$ENDIF}

  S := CRLF + '--' + Bound + '--' + CRLF;

  {$IFDEF USE_SYNAPSE}
  FHttpClient.Document.Write(Pointer(S)^, Length(S));
  FHttpClient.MimeType := 'multipart/form-data, boundary=' + Bound;
  {$ELSE}
  FHttpClient.StreamToSend.Write(Pointer(S)^, Length(S));
  FHttpClient.HeaderToSend.ContentType := 'multipart/form-data, boundary=' + Bound;
  {$ENDIF}
  
  {$IFDEF USE_SYNAPSE}
  Result := FHttpClient.HTTPMethod(HTTP_METHOD_POST, FHttpURL);
  {$ELSE}
  FHttpClient.HeaderToSend.SetFieldByName(HTTP_METHOD, HTTP_METHOD_POST);
  FHttpClient.HeaderToSend.SetFieldByName(HTTP_URL, FHttpURL);
  
  FHttpClient.Send;
  Result := true;
  {$ENDIF}
  
  if Assigned(ReceivedData) then
     begin // N.B. ReceivedData must be from class that overrides the TStream.Size property
       // clear old contents
       ReceivedData.Size := 0;
       // position in the beginning
       ReceivedData.Position := 0;
       // copy new data
       {$IFDEF USE_SYNAPSE}
       ReceivedData.CopyFrom(FHttpClient.Document, 0);
       {$ELSE}
       ReceivedData.CopyFrom(FHttpClient.ReceivedStream, 0);
       {$ENDIF}
       // go to the beginning
       ReceivedData.Position := 0;
     end;
end;

end.

