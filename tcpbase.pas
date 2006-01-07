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

  { TTextSocketStream }

  { TTextInetSocket }

  TTextInetSocket = class(TInetSocket)
  public
    constructor Create(AHandle : Longint); override;
    
    function  RecvText : String;
    procedure SendText(const AText : String);
  end;

implementation

uses Sockets;

{ TTextSocketStream }

constructor TTextInetSocket.Create(AHandle: Longint);
begin
  inherited Create(AHandle);
  SocketOptions := SocketOptions + [soKeepAlive];
end;

function TTextInetSocket.RecvText: String;
var len, n_len : LongInt;
begin
  len := 0;
  n_len := 0;
  SetLength(Result, 0);
  Read(n_len, SizeOf(n_len));
  len := NetToHost(n_len);
  if len > 0 then
     begin
       SetLength(Result, len);
       if Read(Result[1], len) <> len then
          raise Exception.Create('TTextSocketStream.RecvText - FAILED!');
     end;
end;

procedure TTextInetSocket.SendText(const AText: String);
var len, n_len : LongInt;
begin
  len := Length(AText);
  n_len := HostToNet(len);
  Write(n_len, SizeOf(n_len));
  if len > 0 then
     if Write(AText[1], len) <> len then
        raise Exception.Create('TTextSocketStream.SendText - FAILED!');
end;

end.
