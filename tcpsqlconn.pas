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

interface

uses Classes, SysUtils,
     CustomSQLConn, TCPBase;

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
    //todo : still not working
    function  CheckSocketConnected(S : LongInt) : Boolean;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function  GetConnected: Boolean; override;
    function  TCPSendData: Boolean;
    function  Open : Boolean;  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$IFDEF WIN32}
uses WinSock;
{$ELSE}
uses BaseUnix;
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

function TTCPSQLConnection.CheckSocketConnected(S: LongInt): Boolean;
var TimeOut : TTimeVal;
    wSet : TFDSet;
{$IFDEF WIN32}
    Res : tOS_INT;
{$ELSE}
    Res : Integer;
{$ENDIF}
begin
   TimeOut.tv_sec := 0;
   TimeOut.tv_usec := 10; // wait 10ms until select() returns

{$IFDEF WIN32}
   FD_ZERO(wSet);
   FD_SET(S, wSet);
   
   Res := WinSock.Select(0, nil, @wSet, nil, @TimeOut);
   // S is writeable and no errors
   Result := (Res = 1) and (WSAGetLastError = 0);
{$ELSE}
   fpFD_ZERO(wSet);
   fpFD_SET(S, wSet);
   
   Res := FPSelect(0, nil, @wSet, nil, @TimeOut);
   // S is writeable and no errors
   Result := (Res = 1) and (Errno = 0);
{$ENDIF}
end;


procedure TTCPSQLConnection.DoConnect;
begin
  DoSetConnectionParams;
  if Assigned(FSocket) then
     try
       FSocket.Free;
       FSocket := nil;
     except
       // do nothing. catch all exceptions here
     end;
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
begin // still have a valid socket handle
  Result := (FSocket <> nil) and CheckSocketConnected(FSocket.Handle);
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
    strResponse := FSocket.RecvText;

    ssResult := TStringStream.Create(strResponse);

    if Assigned(ReceivedData) then
       begin // N.B. ReceivedData must be from class that overrides the TStream.Size property
         // clear old contents
         ReceivedData.Size := 0;
         // position in the beginning
         ReceivedData.Position := 0;
         // copy new data
         ReceivedData.CopyFrom(ssResult,0);
         // go to the beginning
         ReceivedData.Position := 0;
       end;

    Result := true;
  finally
    ssDataToSend.Free;
    ssResult.Free;
  end;
end;

end.

