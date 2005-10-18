unit customsqlconn;

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
     CustomXMLDataset;

type

  TLoginEvent = procedure(Sender: TObject; Username, Password: string) of object;
  TConnectChangeEvent = procedure(Sender: TObject; Connecting: Boolean) of object;
  TBeforeCommitEvent = procedure(Sender : TObject) of object;
  TBeforeCommitDatasetEvent = procedure(Sender : TObject; Dataset : TCustomXMLDataSet) of object;

  { TCustomSQLConnection - connection and transaction handling }
  TCustomSQLConnection = class(TComponent)
  private
    FConnParams : TStrings;   // Used to handle sending / receiving
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
    FBeforeCommit : TBeforeCommitEvent;
    FBeforeCommitDataset : TBeforeCommitDatasetEvent;
  protected
    FInTransaction : Boolean; // transaction handling
    FTransactXMLList : TList; // used for internal cache of XML during transactions
    procedure DoConnect; virtual;             // descendants must override this
    procedure DoDisconnect; virtual;          // descendants must override this
    function  GetConnected: Boolean; virtual; // descendants must override this
    function  GetDataSet(Index: Integer): TCustomXMLDataSet; virtual;
    function  GetDataSetCount: Integer; virtual;
    procedure Loaded; override;
    procedure SetConnected(Value: Boolean); virtual;
    procedure SendConnectEvent(Connecting: Boolean);
    property  StreamedConnected: Boolean read FStreamedConnected write FStreamedConnected;
  public
    DataToSend   : TStream;     // data that is being passed over the connection
    ReceivedData : TStream;
    procedure RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil); virtual;
    procedure UnRegisterClient(Client: TObject); virtual;
    {--------------------------------------------------------------------------}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {--------------------------------------------------------------------------}
    function  Open : Boolean;  virtual;
    procedure Close; virtual;
    { ----- transaction handling ----- }
    procedure StartTransaction; virtual;
    procedure Rollback; virtual;
    procedure Commit; virtual;
    property  InTransaction : Boolean read FInTransaction;
    {--------------------------------------------------------------------------}
    property Connected: Boolean read GetConnected write SetConnected default False;
    property DataSets[Index: Integer]: TCustomXMLDataSet read GetDataSet;
    property DataSetCount: Integer read GetDataSetCount;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt default False;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
    property BeforeCommit : TBeforeCommitEvent read FBeforeCommit write FBeforeCommit;
    property BeforeCommitDataset : TBeforeCommitDatasetEvent read FBeforeCommitDataset write FBeforeCommitDataset;
  published
    property ConnParams : TStrings read FConnParams write FConnParams;
  end;

implementation

uses DOM, XMLRead;

(*******************************************************************************
{ TCustomSQLConnection }
*******************************************************************************)

procedure TCustomSQLConnection.DoConnect;
begin
  raise Exception.Create('TCustomSQLConnection.DoConnect - not implemented');
end;

procedure TCustomSQLConnection.DoDisconnect;
begin
  raise Exception.Create('TCustomSQLConnection.DoDisconnect - not implemented');
end;

function TCustomSQLConnection.GetConnected: Boolean;
begin
  Result := False;
end;

function TCustomSQLConnection.GetDataSet(Index: Integer): TCustomXMLDataSet;
begin
  Result := TCustomXMLDataSet(FDataSets.Items[Index]^);
end;

function TCustomSQLConnection.GetDataSetCount: Integer;
begin
  Result := FDataSets.Count;
end;

procedure TCustomSQLConnection.Loaded;
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

procedure TCustomSQLConnection.RegisterClient(Client: TObject; Event: TConnectChangeEvent);
begin
//todo : fix - possible registration of non TCustomXMLDataset clients
// FClients and FDataSets are the same
  FClients.Add(Client);
  FConnectEvents.Add(TMethod(Event).Code);
  if (Client is TCustomXMLDataSet) then
    begin
      FDataSets.Add(Client);
      FTransactXMLList.Add(TXMLDocument.Create);
    end;
end;

procedure TCustomSQLConnection.SetConnected(Value: Boolean);
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

procedure TCustomSQLConnection.SendConnectEvent(Connecting: Boolean);
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
      if TObject(FClients.Items[i]^) is TCustomXMLDataSet then
         TCustomXMLDataSet(FClients.Items[i]^).DataEvent(deConnectChange, Ptrint(Connecting));
*)
    end;
end;

procedure TCustomSQLConnection.UnRegisterClient(Client: TObject);
var Index: Integer;
    P : Pointer;
begin
//todo : fix - possible unregistration of non TCustomXMLDataset clients
// FClients and FDataSets are the same
  if (Client is TCustomXMLDataSet) then
    begin
     Index := FDataSets.IndexOf(Client);
//todo: check deleting of internal XML documents
     if Index <> -1 then
       begin
         P := FTransactXMLList.Items[Index];
         FTransactXMLList.Delete(Index);
         TXMLDocument(P^).Free;
       end;
     FDataSets.Remove(Client);
    end;
  Index := FClients.IndexOf(Client);
  if Index <> -1 then
    begin
      FClients.Delete(Index);
      FConnectEvents.Delete(Index);
    end;
end;

constructor TCustomSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSets := TList.Create;
  FClients := TList.Create;
  FConnectEvents := TList.Create;
  FConnParams := TStringList.Create;
  FInTransaction := false;
  FTransactXMLList := TList.Create;
end;

destructor TCustomSQLConnection.Destroy;
var i : Integer;
begin
  SetConnected(False);
  FreeAndNil(FConnectEvents);
  FreeAndNil(FClients);
  FreeAndNil(FDataSets);
  FConnParams.Free;

//todo : check this out
  for i := 0 to FTransactXMLList.Count - 1 do
     TXMLDocument(FTransactXMLList.Items[i]^).Free;
  FreeAndNil(FTransactXMLList);
  
  inherited Destroy;
end;

function TCustomSQLConnection.Open : Boolean;
begin
  SetConnected(True);
  Result := true;
end;

procedure TCustomSQLConnection.Close;
begin
  SetConnected(False);
end;

procedure TCustomSQLConnection.StartTransaction;
var i : Integer;
begin
  if InTransaction then
     raise Exception.Create('Transaction is active!');
     
  if (FTransactXMLList.Count <> FDataSets.Count) then
     raise Exception.Create('Can not start transaction. Internal count differs!');
     
  for i := 0 to FTransactXMLList.Count - 1 do
    if Assigned(TXMLDocument(FTransactXMLList.Items[i]^).DocumentElement) then
      with TXMLDocument(FTransactXMLList.Items[i]^) do
        begin
          RemoveChild(DocumentElement);
          AppendChild(TCustomXMLDataset(FDataSets.Items[i]^).XMLDocument.DocumentElement);
        end;

  FInTransaction := true;
end;

procedure TCustomSQLConnection.Rollback;
var i : Integer;
begin
  if (FTransactXMLList.Count <> FDataSets.Count) then
     raise Exception.Create('Can not rollback. Internal count differs!');

  for i := 0 to FTransactXMLList.Count - 1 do
    if Assigned(TCustomXMLDataset(FDataSets.Items[i]^).XMLDocument.DocumentElement) then
      with TCustomXMLDataset(FDataSets.Items[i]^).XMLDocument do
        begin
          RemoveChild(DocumentElement);
          AppendChild(TXMLDocument(FTransactXMLList.Items[i]^).DocumentElement);
        end;
  FInTransaction := false;
end;

procedure TCustomSQLConnection.Commit;
//todo : fix time out / connection errors

// todo : N.B. COMMIT always returns a valid XML file
// the other side of the connection must supply the result XML file
// XML for all datasets is sent step by step
var i : Integer;
begin
  if Assigned(FBeforeCommit) then
     FBeforeCommit(Self);
     
  for i := 0 to FDataSets.Count - 1 do
    begin // send current xml
      if Assigned(FBeforeCommitDataset) then
         FBeforeCommitDataset(Self, TCustomXMLDataset(FDataSets.Items[i]^));
         
      DataToSend := TCustomXMLDataset(FDataSets.Items[i]^).XMLStringStream;
      Open;
      ReadXMLFile(TCustomXMLDataset(FDataSets.Items[i]^).XMLDocument,ReceivedData);
    end;
  FInTransaction := false;
end;

end.

