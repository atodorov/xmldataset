unit sqlconnection;

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

interface

uses Classes, SysUtils,
     {$IFDEF WIN32} //todo : change this to only HTTPClient
     HTTPSend in 'synapse/source/lib/httpsend.pas',
     {$ELSE}
       {$IFDEF LINUX}
       HTTPClient,
       {$ENDIF}
     {$ENDIF}
     BaseXMLDataset;


const
  // xml processor parameters constants
  XML_PROCESSOR_URL = 'url';
  SQL_CONNECTION_STRING = 'connstr';

  HTTP_USER_AGENT = 'user-agent';
  HTTP_URL = 'url';
  HTTP_METHOD = 'method'; // allowed values are POST, GET
  HTTP_METHOD_POST = 'POST';
  HTTP_METHOD_GET  = 'GET';
  HTTP_COOKIES = 'cookies';
  
type

  TLoginEvent = procedure(Sender: TObject; Username, Password: string) of object;
  TConnectChangeEvent = procedure(Sender: TObject; Connecting: Boolean) of object;

  { TBaseSQLConnection - connection and transaction handling }

  TBaseSQLConnection = class(TComponent)
  private
    FInTransaction : Boolean;           // transaction handling
    FXMLProcessorParameters : TStrings; // XML processor parameters. Used to handle sending / receiving
    FClients: TList;
    FDataSets: TList;
    FConnectEvents: TList;
    FLoginPrompt: Boolean;
    FStreamedConnected: Boolean;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FOnLogin: TLoginEvent;
  protected
    procedure DoConnect; virtual;             // descendants must override this
    procedure DoDisconnect; virtual;          // descendants must override this
    function  GetConnected: Boolean; virtual; // descendants must override this
    function  GetDataSet(Index: Integer): TBaseXMLDataSet; virtual;
    function  GetDataSetCount: Integer; virtual;
    procedure Loaded; override;
    procedure SetConnected(Value: Boolean); virtual;
    procedure SendConnectEvent(Connecting: Boolean);
    property  StreamedConnected: Boolean read FStreamedConnected write FStreamedConnected;
  public
    // moved to public
    procedure RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil); virtual;
    procedure UnRegisterClient(Client: TObject); virtual;
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;  virtual;
    procedure Close; virtual;
    property Connected: Boolean read GetConnected write SetConnected default False;
    property DataSets[Index: Integer]: TBaseXMLDataSet read GetDataSet;
    property DataSetCount: Integer read GetDataSetCount;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt default False;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
  published
    property XMLProcessorParameters : TStrings read FXMLProcessorParameters write FXMLProcessorParameters;
  end;
  
  { THTTPSQLConnection }
  THTTPSQLConnection = class(TBaseSQLConnection)
  private
    FHttpClient : {$IFDEF WIN32} THTTPSend; {$ELSE} {$IFDEF LINUX} THTTPClient; {$ENDIF} {$ENDIF}
    FHttpMethod : String;
    FURL : String;
    function  GetDocument: TMemoryStream;
    function  GetUserAgent: String;
    procedure SetDocument(const AValue: TMemoryStream);
    procedure SetUserAgent(const AValue: String);
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
    // XML document that is being sent / received
    property Document: TMemoryStream read GetDocument write SetDocument;
  published
    // connection properties
    property UserAgent : String read GetUserAgent write SetUserAgent;
    property HttpMethod : String read FHttpMethod write FHttpMethod;
    property HttpUrl : String read FURL write FURL;
  end;

implementation


(*******************************************************************************
{ TBaseSQLConnection }
*******************************************************************************)

procedure TBaseSQLConnection.DoConnect;
begin
  raise Exception.Create('TBaseSQLConnection.DoConnect - not implemented');
end;

procedure TBaseSQLConnection.DoDisconnect;
begin
  raise Exception.Create('TBaseSQLConnection.DoDisconnect - not implemented');
end;

function TBaseSQLConnection.GetConnected: Boolean;
begin
  Result := False;
end;

function TBaseSQLConnection.GetDataSet(Index: Integer): TBaseXMLDataSet;
begin
  Result := TBaseXMLDataSet(FDataSets.Items[Index]^);
end;

function TBaseSQLConnection.GetDataSetCount: Integer;
begin
  Result := FDataSets.Count;
end;

procedure TBaseSQLConnection.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedConnected then SetConnected(True);
  except
    if (csDesigning in ComponentState)
       then if Assigned(Classes.ApplicationHandleException)
               then Classes.ApplicationHandleException(ExceptObject)
                else ShowException(ExceptObject, ExceptAddr)
       else raise;
  end;
end;

procedure TBaseSQLConnection.RegisterClient(Client: TObject; Event: TConnectChangeEvent);
begin
  FClients.Add(Client);
  FConnectEvents.Add(TMethod(Event).Code);
  if Client is TBaseXMLDataSet then
     FDataSets.Add(Client);
end;

procedure TBaseSQLConnection.SetConnected(Value: Boolean);
begin
  if (csReading in ComponentState) and Value
   then FStreamedConnected := True
   else
     begin
       if Value = GetConnected then Exit;
       if Value then
         begin
           if Assigned(BeforeConnect) then BeforeConnect(Self);
           DoConnect;
           SendConnectEvent(True);
           if Assigned(AfterConnect) then AfterConnect(Self);
         end
       else
         begin
           if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
           SendConnectEvent(False);
           DoDisconnect;
           if Assigned(AfterDisconnect) then AfterDisconnect(Self);
         end;
     end;
end;

procedure TBaseSQLConnection.SendConnectEvent(Connecting: Boolean);
var i : Integer;
    ConnectEvent: TConnectChangeEvent;
begin
  for i := 0 to FClients.Count - 1 do
    begin
      if FConnectEvents.Items[i] <> nil then
         begin
           TMethod(ConnectEvent).Code := FConnectEvents.Items[i];
           TMethod(ConnectEvent).Data := FClients.Items[i];
           ConnectEvent(Self, Connecting);
         end;
// todo : fix this deConnectChanged
// A client must re-connect and get new XML ?????
(*
      if TObject(FClients.Items[i]^) is TBaseXMLDataSet then
         TBaseXMLDataSet(FClients.Items[i]^).DataEvent(deConnectChange, Ptrint(Connecting));
*)
    end;
end;

procedure TBaseSQLConnection.UnRegisterClient(Client: TObject);
var Index: Integer;
begin
  if Client is TBaseXMLDataSet then
     FDataSets.Remove(Client);
  Index := FClients.IndexOf(Client);
  if Index <> -1 then
    begin
      FClients.Delete(Index);
      FConnectEvents.Delete(Index);
    end;
end;

constructor TBaseSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSets := TList.Create;
  FClients := TList.Create;
  FConnectEvents := TList.Create;
  FXMLProcessorParameters := TStringList.Create;
end;

destructor TBaseSQLConnection.Destroy;
begin
  SetConnected(False);
  FreeAndNil(FConnectEvents);
  FreeAndNil(FClients);
  FreeAndNil(FDataSets);
  FXMLProcessorParameters.Free;
  inherited Destroy;
end;

procedure TBaseSQLConnection.Open;
begin
  SetConnected(True);
end;

procedure TBaseSQLConnection.Close;
begin
  SetConnected(False);
end;

{ THTTPSQLConnection }

function THTTPSQLConnection.GetUserAgent: String;
begin
  Result := FHttpClient.UserAgent;
end;

function THTTPSQLConnection.GetDocument: TMemoryStream;
begin
  Result := FHttpClient.Document;
end;

procedure THTTPSQLConnection.SetDocument(const AValue: TMemoryStream);
begin
  FHttpClient.Document.CopyFrom(AValue,0); // copy entire document
end;

procedure THTTPSQLConnection.SetUserAgent(const AValue: String);
begin
  FHttpClient.UserAgent := AValue;
end;

procedure THTTPSQLConnection.DoSetConnectionParams;
var index : Integer;
begin
// USER AGENT
  index := XMLProcessorParameters.IndexOfName(HTTP_USER_AGENT);
  if (index > -1) then UserAgent := XMLProcessorParameters.ValueFromIndex[index];
// HTTP_METHOD
  index := XMLProcessorParameters.IndexOfName(HTTP_METHOD);
  if (index > -1) then HttpMethod := XMLProcessorParameters.ValueFromIndex[index];
// HTTP_URL
  index := XMLProcessorParameters.IndexOfName(HTTP_URL);
  if (index > -1) then HttpUrl := XMLProcessorParameters.ValueFromIndex[index];
  
end;

procedure THTTPSQLConnection.DoConnect;
begin
  DoSetConnectionParams;
  
  
end;

procedure THTTPSQLConnection.DoDisconnect;
begin

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
  FHttpMethod := HTTP_METHOD_POST;
  FURL := '';
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

end.

