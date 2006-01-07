unit tcpsqlconn;

{$mode objfpc}{$H+}

{*******************************************************************************

 This file is part of the XMLDataset suite for the Free Component Library!
 Connection class that sends data over TCP / IP.

 (c) 2006 by Alexander Todorov.
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

uses Classes, SysUtils, CustomSQLConn, TCPBase;

const
  TCP_HOST = 'host';
  TCP_PORT = 'port';
  TCP_TIMEOUT = 'timeout';

type

  { TTCPSQLConnection }
  TTCPSQLConnection = class(TCustomSQLConnection)
  private
    FSocket : TTextInetSocket;
    FHost : String;
    FPort : Word;
    FTimeOut : Integer;
    procedure DoSetConnectionParams;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function  GetConnected: Boolean; override;
    function  TCPSendData: Boolean;
    function Open : Boolean;  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //todo : remove if not needed
//    property Cookies : TStringList // http connection cookies - session management
  end;

implementation

{$IFDEF DEBUGCONNECTION}
uses Dialogs;
{$ENDIF}


(*******************************************************************************
{ TTCPSQLConnection }
*******************************************************************************)

procedure TTCPSQLConnection.DoSetConnectionParams;
var index : Integer;
begin
// TCP_HOST
  index := ConnParams.IndexOfName(TCP_HOST);
  if (index > -1)
    then FHost := ConnParams.ValueFromIndex[index]
    else raise Exception.Create('Required parameter '+TCP_HOST+' is missing!');

// TCP_PORT
  index := ConnParams.IndexOfName(TCP_PORT);
  if (index > -1)
    then FPort := StrToInt(ConnParams.ValueFromIndex[index])
    else raise Exception.Create('Required parameter '+TCP_PORT+' is missing!');
    
// TCP_TIMEOUT
  index := ConnParams.IndexOfName(TCP_TIMEOUT);
  if (index > -1) then
     FTimeOut := StrToInt(ConnParams.ValueFromIndex[index]);
end;

procedure TTCPSQLConnection.DoConnect;
begin
  DoSetConnectionParams;
  if not Assigned(FSocket) then
     FSocket := TTextInetSocket.Create(FHost, FPort); // connection is made upon creation
end;

procedure TTCPSQLConnection.DoDisconnect;
begin
   if Assigned(FSocket) then
      FSocket.Free;
   FSocket := nil;
   
   DataToSend   := nil;
   ReceivedData := nil;
end;

function TTCPSQLConnection.GetConnected: Boolean;
begin
  Result := (FSocket <> nil) and Assigned(FSocket);
end;

constructor TTCPSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSocket := nil;
  FHost := '127.0.0.1';
  FPort := 0;
  FTimeOut := 60;
end;

destructor TTCPSQLConnection.Destroy;
begin
  if Connected then
     Close;
  inherited Destroy;
end;

function TTCPSQLConnection.Open : Boolean;
begin
  DoSetConnectionParams;
  inherited Open;
  Result := TCPSendData;
end;

function TTCPSQLConnection.TCPSendData: Boolean;
var ssDataToSend, ssResult : TStringStream;
    strResponse : String;
begin
//todo : add some exception handling here
  Result := false;
  DoSetConnectionParams;

  try
    ssDataToSend := TStringStream.Create('');
    ssDataToSend.CopyFrom(DataToSend, 0);

    FSocket.SendText(ssDataToSend.DataString);
//todo : do we have to wait until data is processed
//    sleep(10);
    strResponse := FSocket.RecvText;
    
    ssResult := TStringStream.Create(strResponse);
    
    if Assigned(ReceivedData) then
       ReceivedData.CopyFrom(ssResult,0);

    Result := true;
  finally
    ssDataToSend.Free;
    ssResult.Free;
  end;
end;

end.

