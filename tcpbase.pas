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
  Classes, SysUtils, SSockets;
  

type

  { TTextInetSocket }

  TTextInetSocket = class(TInetSocket)
  public
    constructor Create(AHandle : Longint); override;
    
    function  RecvText : String;
    procedure SendText(const AText : String);
  end;

implementation

uses Sockets;

{ TTextInetSocket }

constructor TTextInetSocket.Create(AHandle: Longint);
begin
  inherited Create(AHandle);
  SocketOptions := SocketOptions + [soKeepAlive];
end;

function TTextInetSocket.RecvText : String;
var len, n_len, r : LongInt;
begin
  r := 0;
  len := 0;
  n_len := 0;
  SetLength(Result, 0);

  if (Read(n_len, SizeOf(n_len)) <> SizeOf(n_len)) and
     (SocketError <> 0) then
     raise Exception.Create('TTextInetSocket.RecvText - SOCKET ERROR '+IntToStr(SocketError));
     
  len := NetToHost(n_len);
  if len > 0 then
     begin
       SetLength(Result, len);
       repeat
         r := r + Read(Result[1+r], len-r);
         if (SocketError <> 0) then
            raise Exception.Create('TTextInetSocket.RecvText - SOCKET ERROR '+IntToStr(SocketError));
       until r >= len;
       if r > len then
          raise Exception.Create('TTextInetSocket.RecvText - more bytes read!');
     end;
end;

procedure TTextInetSocket.SendText(const AText: String);
var len, n_len, w : LongInt;
begin
  w := 0;
  len := Length(AText);
  n_len := HostToNet(len);
  
  if (Write(n_len, SizeOf(n_len)) <> SizeOf(n_len)) and
     (SocketError <> 0) then
     raise Exception.Create('TTextInetSocket.SendText - SOCKET ERROR '+IntToStr(SocketError));

  if len > 0 then
     begin
       repeat
         w := w + Write(AText[1+w], len-w);
         if (SocketError <> 0) then
            raise Exception.Create('TTextInetSocket.SendText - SOCKET ERROR '+IntToStr(SocketError));
       until w = len;
       if w > len then
          raise Exception.Create('TTextInetSocket.SendText - more bytes read!');
     end;
end;

end.
