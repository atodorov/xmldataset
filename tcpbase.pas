unit tcpbase;

{$mode objfpc}{$H+}

{*******************************************************************************

 Base classes for TCP/IP and Unix sockets communications!
 This is a copy from SSockets implementation, but all classes are derived from
 TTextSocketStream which knows how to handle text.tcpbase Use at your own risk!

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
  Classes, SysUtils, SSockets;

type

  { TTextSocketStream }
 
  TTextSocketStream = class(TSocketStream)
  public
    function  RecvText : String; virtual;
    procedure SendText(const AText : String); virtual;
  end;

  { TTextInetSocket }

  TTextInetSocket = class(TTextSocketStream)
  private
    FHost : String;
    FPort : Word;
  protected
    procedure DoConnect(ASocket : longint); virtual;
  public
    constructor Create(ASocket : longint); override; overload;
    constructor Create(const AHost: String; APort: Word); overload;
    property Host : String read FHost;
    property Port : Word read FPort;
  end;

  {$IFDEF UNIX}

  { TTextUnixSocket }
  TTextUnixSocket = class(TTextSocketStream)
  private
    FFileName : String;
  protected
    procedure DoConnect(ASocket : longint); virtual;
  public
    constructor Create(ASocket : Longint); overload;
    constructor Create(AFileName : String); overload;
    property FileName : String read FFileName;
  end;
 {$ENDIF}


implementation

uses Sockets, Resolve;

{ TTextSocketStream }
 
function TTextSocketStream.RecvText : String;
var len, n_len, r : LongInt;
begin
  r := 0;
  len := 0;
  n_len := 0;
  SetLength(Result, 0);

  if (Read(n_len, SizeOf(n_len)) <> SizeOf(n_len)) then
     raise Exception.Create(ClassName+'.RecvText - SOCKET ERROR '+IntToStr(SocketError));
     
  len := NetToHost(n_len);
  if len > 0 then
     begin
       SetLength(Result, len);
       repeat
         r := r + Read(Result[1+r], len-r);
         if (SocketError <> 0) then
            raise Exception.Create(ClassName+'.RecvText - SOCKET ERROR '+IntToStr(SocketError));
       until r >= len;
       if r > len then
          raise Exception.Create(ClassName+'.RecvText - more bytes read!');
     end;
end;

procedure TTextSocketStream.SendText(const AText: String);
var len, n_len, w : LongInt;
begin
  w := 0;
  len := Length(AText);
  n_len := HostToNet(len);
  
  if (Write(n_len, SizeOf(n_len)) <> SizeOf(n_len)) and
     (SocketError <> 0) then
     raise Exception.Create(ClassName+'.SendText - SOCKET ERROR '+IntToStr(SocketError));

  if len > 0 then
     begin
       repeat
         w := w + Write(AText[1+w], len-w);
         if (SocketError <> 0) then
            raise Exception.Create(ClassName+'.SendText - SOCKET ERROR '+IntToStr(SocketError));
       until w = len;
       if w > len then
          raise Exception.Create(ClassName+'.SendText - more bytes read!');
     end;
end;

{ TTextInetSocket }

procedure TTextInetSocket.DoConnect(ASocket: longint);
var A : THostAddr;
    addr: TInetSockAddr;
begin
  A := StrToNetAddr(FHost);
  if A.s_bytes[4] = 0 then
    with THostResolver.Create(Nil) do
      try
        if not NameLookup(FHost) then
           raise ESocketError.Create(seHostNotFound, [FHost]);
        A := HostAddress;
      finally
        free;
      end;
  addr.family := AF_INET;
  addr.port := ShortHostToNet(FPort);
  addr.addr := a.s_addr; // hosttonet(A).s_addr;
//Cardinal(A);

  if not Sockets.Connect(ASocket, addr, sizeof(addr)) then
    raise ESocketError.Create(seConnectFailed, [Format('%s:%d',[FHost, FPort])]);
end;

constructor TTextInetSocket.Create(ASocket: Longint);
begin
  inherited Create(ASocket);
  SocketOptions := SocketOptions + [soKeepAlive];
end;

constructor TTextInetSocket.Create(const AHost: String; APort: Word);
var S : Longint;
begin
  FHost := AHost;
  FPort := APort;
  S := Socket(AF_INET,SOCK_STREAM,0);
  DoConnect(S);
  inherited Create(S);
end;

{$IFDEF UNIX}

{ TTextUnixSocket }
procedure TTextUnixSocket.DoConnect(ASocket: longint);
var UnixAddr : TUnixSockAddr;
    AddrLen  : longint;
begin
  Str2UnixSockAddr(FFilename,UnixAddr,AddrLen);
  if not Connect(ASocket,UnixAddr,AddrLen) then
    raise ESocketError.Create(seConnectFailed,[FFilename]);
end;

constructor TTextUnixSocket.Create(ASocket: Longint);
begin
  inherited Create(ASocket);
end;

constructor TTextUnixSocket.Create(AFileName: String);
var S : Longint;
begin
  FFileName:=AFileName;
  S:=Socket(AF_UNIX,SOCK_STREAM,0);
  DoConnect(S);
  inherited Create(S);
end;
{$ENDIF}

end.
