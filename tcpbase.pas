unit tcpbase;

{$mode objfpc}{$H+}

{*******************************************************************************

 Base classes for TCP / IP communications! Use at your own risk!

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

uses
  Classes, SysUtils, Sockets;

type

  { TCustomTCPObject }
  TCustomTCPObject = class (TObject)
  private
    FTimeOut : Integer;
    FConnected : Boolean;
    FHost : String;  // host name or ip address
    FPort : Word;
    FSocket : Longint ; // communication socket
    function Get_Connected: Boolean;
  protected
    function GetErrMsg(const AMethodName, Msg : String) : String;
  public
    function ReadBuffer(var Buffer; Count: LongInt): LongInt;
    function WriteBuffer(const Buffer; Count: LongInt): LongInt;

    constructor Create; virtual;
    destructor Destroy; override;
    // todo : write try-except-raise for ReceiveText, SendText and Connect
    function  ReceiveText:String; virtual;
    procedure SendText(const AText:String);

    procedure Disconnect; virtual;

    property Host : String  read FHost write FHost;
    property Port : Word    read FPort write FPort;
    property Connected : Boolean read Get_Connected;
  end;
  
  TTCPClient = class (TCustomTCPObject)
  public
    { connect to server socket }
    function Connect(const ATimeOut : Integer) : Boolean; virtual;
  end;
  
  { TTCPServer }

  TTCPServer = class (TCustomTCPObject)
  private
    FListening : Boolean;
    FListeningSocket : LongInt;
    function Get_Listening: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Disconnect; override;
    
    function Bind : Boolean;
    function Listen(const MaxConn : LongInt) : Boolean;
    function Accept : Boolean;
    function ShutDown : Boolean;
    
    property Listening : Boolean read Get_Listening;
  end;

implementation

{$IFNDEF WIN32}
uses Errors, Resolve;
{$ENDIF WIN32}

{ TCustomTCPObject }

function TCustomTCPObject.Get_Connected: Boolean;
begin
  Result := FConnected;
end;

function TCustomTCPObject.GetErrMsg(const AMethodName, Msg : String): String;
begin
   Result := Self.ClassName + '.' + AMethodName + ' - ' + Msg;
   {$IFDEF WIN32}
   Result := Format(Result + ' %d', [SocketError]);
   {$ELSE}
   Result := Format(Result + ' %d : %s', [SocketError, strerror(SocketError)]);
   {$ENDIF WIN32}
end;

function TCustomTCPObject.ReadBuffer(var Buffer; Count: LongInt): LongInt;
var
  p: ^char;
  nr: integer;
begin
   try
     p := @Buffer;
     Result := 0;
     repeat
       nr := Recv(FSocket, p^, Count-Result, 0);
       if (SocketError <> 0) then
          raise Exception.Create(GetErrMsg('ReadBuffer','ERROR'));

       if nr = 0 then
          raise Exception.Create('TCustomTCPObject.ReadBuffer - ERROR socket disconnected!');

       Result := Result + nr;
       inc(p, nr);
     until Result = Count;

   except
     on E : Exception do
       begin
         Disconnect;
         raise;
       end;
   end;
end;

function TCustomTCPObject.WriteBuffer(const Buffer; Count: LongInt): LongInt;
var
  nw: Integer;
  p: ^Char;
begin
   try

     p := @Buffer;
     Result := 0;
     repeat
       nw := Send(FSocket, p^, Count-Result, MSG_NOSIGNAL);
       if (SocketError <> 0) then
          raise Exception.Create(GetErrMsg('WriteBuffer', 'ERROR'));

       if nw = 0 then
         raise Exception.Create('TCustomTCPObject.WriteBuffer - ERROR socket disconnected!');

       Result := Result + nw;
       inc(p, nw);
     until Result = Count;

   except
     on E : Exception do
       begin
         Disconnect;
         raise;
       end;
   end;
end;

constructor TCustomTCPObject.Create;
begin
  inherited Create;
  FSocket := 0;
  FConnected := false;
  FHost := '127.0.0.1';
  FPort := 0;
end;

destructor TCustomTCPObject.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TCustomTCPObject.ReceiveText : String;
var len : LongInt;
begin
  if Connected then
     begin
       len := 0;
       SetLength(Result, 0);
       ReadBuffer(len, SizeOf(len));
       len := NToHl(len);
       if len > 0 then
          begin
            SetLength(Result, len);
            if ReadBuffer(Result[1], len) <> len then
               raise Exception.Create('TCustomTCPObject.ReceiveText - internal error!');
          end;
     end
  else raise Exception.Create('TCustomTCPObject.ReceiveText - Connection not active!');
end;

procedure TCustomTCPObject.SendText(const AText: String);
var len : LongInt;
begin
  if Connected then
     begin
       len := Length(AText);
       len := htonl(len);
       WriteBuffer(len, SizeOf(len));
       if len > 0 then
          WriteBuffer(AText[1], len);
     end
  else raise Exception.Create('TCustomTCPObject.SendText - Connection not active!');
end;

procedure TCustomTCPObject.Disconnect;
begin
  CloseSocket(FSocket);
  FConnected := false;
end;

{ TTCPClient }

function TTCPClient.Connect(const ATimeOut: Integer) : Boolean;
var saAddr : TInetSockAddr;
{$IFNDEF WIN32}
    hr : THostResolver;
{$ENDIF WIN32}
begin
  FTimeOut := ATimeOut;

  if LowerCase(Host) = 'localhost' then // patch to resolving
     Host := '127.0.0.1';

  FSocket := Socket(AF_INET, SOCK_STREAM, 0); // create socket descriptor
  if (FSocket <= 0) then
    raise Exception.Create(GetErrMsg('Connect','Creating socket failed - ERROR'));

  // construct address
  saAddr.sin_family := AF_INET;
  saAddr.sin_port := htons(Port);

  saAddr.sin_addr.s_addr := StrToNetAddr(Host).s_addr;
  if (saAddr.sin_addr.s_addr and $FF) = 0 then
    {$IFNDEF WIN32}
    try // try to resolve host name
      hr := THostResolver.Create(nil);
      if not hr.NameLookup(Host) then
         raise Exception.Create('TTCPClient.Connect - host not found');

      saAddr.sin_addr.s_addr := htonl(hr.HostAddress.s_addr);
    finally
      hr.Free;
    end;
    {$ELSE}
    raise Exception.Create('TTCPClient.Connect - invalid ip address or host name!');
    {$ENDIF WIN32}

  // connect to socket
  Result := Sockets.Connect(FSocket,saAddr,SizeOf(saAddr));
  if (not Result) or (SocketError <> 0) then
    raise Exception.Create(GetErrMsg('Connect', 'Connection error -'));

  FConnected := true;
end;

{ TTCPServer }

function TTCPServer.Get_Listening: Boolean;
begin
  Result := FListening;
end;

constructor TTCPServer.Create;
begin
  inherited Create;
  FListening := false;
end;

destructor TTCPServer.Destroy;
begin
  inherited Destroy;
  FListening := false;
end;

procedure TTCPServer.Disconnect;
begin
 inherited Disconnect;
 CloseSocket(FListeningSocket);
 FListening := false;
end;

function TTCPServer.Bind : Boolean;
var saAddr : TInetSockAddr;
{$IFNDEF WIN32}
    hr : THostResolver;
{$ENDIF WIN32}
begin
  if LowerCase(Host) = 'localhost' then // patch to resolving
     Host := '127.0.0.1';

  FListeningSocket := Socket(AF_INET, SOCK_STREAM, 0); // create socket descriptor
  if (FListeningSocket <= 0) or (SocketError <> 0) then
    raise Exception.Create(GetErrMsg('Bind', 'Creating socket failed - ERROR'));

  // construct address
  saAddr.sin_family := AF_INET;
  saAddr.sin_port := htons(Port);

  saAddr.sin_addr.s_addr := StrToNetAddr(Host).s_addr;
  if (saAddr.sin_addr.s_addr and $FF) = 0 then
    {$IFNDEF WIN32}
    try // try to resolve host name
      hr := THostResolver.Create(nil);
      if not hr.NameLookup(Host) then
         raise Exception.Create('TTCPServer.Bind - host not found');

      saAddr.sin_addr.s_addr := htonl(hr.HostAddress.s_addr);
    finally
      hr.Free;
    end;
    {$ELSE}
    raise Exception.Create('TTCPServer.Connect - invalid ip address or host name!');
    {$ENDIF WIN32}

  // bind to socket
  Result := Sockets.Bind(FListeningSocket, saAddr, SizeOf(saAddr));
  if (not Result) or (SocketError <> 0) then
    raise Exception.Create(GetErrMsg('Bind', 'Connection error - ERROR'));
    
// todo : add a flag to show that bind is active
end;

function TTCPServer.Listen(const MaxConn : LongInt): Boolean;
begin
  Result := Sockets.Listen(FListeningSocket, MaxConn);

  if (not Result) or (SocketError <> 0) then
    raise Exception.Create(GetErrMsg('Listen', 'Connection error - ERROR'));

  FListening := true;
end;

function TTCPServer.Accept : Boolean;
var saAddr : TInetSockAddr;
    addrSize : LongInt;
begin
  Result := false;
  addrSize := SizeOf(saAddr);
  // returns a communication socket with the client. FListeningSockets remains listening
  FSocket := Sockets.Accept(FListeningSocket, saAddr, addrSize);
  if (SocketError <> 0) then
    raise Exception.Create(GetErrMsg('Accept', 'Connection error - ERROR'));

  FConnected := true;
  Result := true;
end;

function TTCPServer.ShutDown : Boolean;
begin
  Result := Sockets.Shutdown(FSocket, 2) = 0; // Sending nor receiving are allowed.
  
  if (not Result) or (SocketError <> 0) then
    raise Exception.Create(GetErrMsg('ShutDown', 'Connection error - ERROR'));
end;

end.
