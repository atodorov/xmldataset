{==============================================================================|
| Project : Ararat Synapse                                       | 002.003.000 |
|==============================================================================|
| Content: POP3 client                                                         |
|==============================================================================|
| Copyright (c)1999-2003, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2001-2003.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(POP3 protocol client)

Used RFC: RFC-1734, RFC-1939, RFC-2195, RFC-2449, RFC-2595
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit pop3send;

interface

uses
  SysUtils, Classes,
  {$IFDEF STREAMSEC}
  TlsInternalServer, TlsSynaSock,
  {$ENDIF}
  blcksock, synautil, synacode;

const
  cPop3Protocol = 'pop3';

type

  {:The three types of possible authorization methods for "logging in" to a POP3
   server.}
  TPOP3AuthType = (POP3AuthAll, POP3AuthLogin, POP3AuthAPOP);

  {:@abstract(Implementation of POP3 client protocol.)

   Note: Are you missing properties for setting Username and Password? Look to
   parent @link(TSynaClient) object!

   Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TPOP3Send = class(TSynaClient)
  private
    {$IFDEF STREAMSEC}
    FSock: TSsTCPBlockSocket;
    FTLSServer: TCustomTLSInternalServer;
    {$ELSE}
    FSock: TTCPBlockSocket;
    {$ENDIF}
    FResultCode: Integer;
    FResultString: string;
    FFullResult: TStringList;
    FStatCount: Integer;
    FStatSize: Integer;
    FTimeStamp: string;
    FAuthType: TPOP3AuthType;
    FPOP3cap: TStringList;
    FAutoTLS: Boolean;
    FFullSSL: Boolean;
    function ReadResult(Full: Boolean): Integer;
    function Connect: Boolean;
    function AuthLogin: Boolean;
    function AuthApop: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    {:Call CAPA command for get POP3 server capabilites.
     note: not all servers support this command!}
    function Capability: Boolean;

    {:Connect to remote POP3 host. If all OK, result is @true.}
    function Login: Boolean;

    {:Disconnects from POP3 server.}
    function Logout: Boolean;

    {:Send RSET command. If all OK, result is @true.}
    function Reset: Boolean;

    {:Send NOOP command. If all OK, result is @true.}
    function NoOp: Boolean;

    {:Send STAT command and fill @link(StatCount) and @link(StatSize) property.
     If all OK, result is @true.}
    function Stat: Boolean;

    {:Send LIST command. If Value is 0, LIST is for all messages. After
     successful operation is listing in FullResult. If all OK, result is @True.}
    function List(Value: Integer): Boolean;

    {:Send RETR command. After successful operation dowloaded message in
     @link(FullResult). If all OK, result is @true.}
    function Retr(Value: Integer): Boolean;

    {:Send DELE command for delete specified message. If all OK, result is @true.}
    function Dele(Value: Integer): Boolean;

    {:Send TOP command. After successful operation dowloaded headers of message
     and maxlines count of message in @link(FullResult). If all OK, result is
     @true.}
    function Top(Value, Maxlines: Integer): Boolean;

    {:Send UIDL command. If Value is 0, UIDL is for all messages. After
     successful operation is listing in FullResult. If all OK, result is @True.}
    function Uidl(Value: Integer): Boolean;

    {:Call STLS command for upgrade connection to SSL/TLS mode.}
    function StartTLS: Boolean;

    {:Try to find given capabily in capabilty string returned from POP3 server
     by CAPA command.}
    function FindCap(const Value: string): string;
  published
    {:Result code of last POP3 operation. 0 - error, 1 - OK.}
    property ResultCode: Integer read FResultCode;

    {:Result string of last POP3 operation.}
    property ResultString: string read FResultString;

    {:Stringlist with full lines returned as result of POP3 operation. I.e. if
     operation is LIST, this property is filled by list of messages. If
     operation is RETR, this property have downloaded message.}
    property FullResult: TStringList read FFullResult;

    {:After STAT command is there count of messages in inbox.}
    property StatCount: Integer read FStatCount;

    {:After STAT command is there size of all messages in inbox.}
    property StatSize: Integer read  FStatSize;

    {:If server support this, after comnnect is in this property timestamp of
     remote server.}
    property TimeStamp: string read FTimeStamp;

    {:Type of authorisation for login to POP3 server. Dafault is autodetect one
     of possible authorisation. Autodetect do this:

     If remote POP3 server support APOP, try login by APOP method. If APOP is
     not supported, or if APOP login failed, try classic USER+PASS login method.}
    property AuthType: TPOP3AuthType read FAuthType Write FAuthType;

    {:If is set to @true, then upgrade to SSL/TLS mode if remote server support it.}
    property AutoTLS: Boolean read FAutoTLS Write FAutoTLS;

    {:SSL/TLS mode is used from first contact to server. Servers with full
     SSL/TLS mode usualy using non-standard TCP port!}
    property FullSSL: Boolean read FFullSSL Write FFullSSL;
{$IFDEF STREAMSEC}
    property Sock: TSsTCPBlockSocket read FSock;
    property TLSServer: TCustomTLSInternalServer read FTLSServer write FTLSServer;
{$ELSE}
    {:Socket object used for TCP/IP operation. Good for seting OnStatus hook, etc.}
    property Sock: TTCPBlockSocket read FSock;
{$ENDIF}
  end;

implementation

constructor TPOP3Send.Create;
begin
  inherited Create;
  FFullResult := TStringList.Create;
  FPOP3cap := TStringList.Create;
{$IFDEF STREAMSEC}           
  FTLSServer := GlobalTLSInternalServer;     
  FSock := TSsTCPBlockSocket.Create;
  FSock.BlockingRead := True;
{$ELSE}
  FSock := TTCPBlockSocket.Create;
{$ENDIF}
  FSock.ConvertLineEnd := true;
  FTimeout := 60000;
  FTargetPort := cPop3Protocol;
  FStatCount := 0;
  FStatSize := 0;
  FAuthType := POP3AuthAll;
  FAutoTLS := False;
  FFullSSL := False;
end;

destructor TPOP3Send.Destroy;
begin
  FSock.Free;
  FPOP3cap.Free;
  FullResult.Free;
  inherited Destroy;
end;

function TPOP3Send.ReadResult(Full: Boolean): Integer;
var
  s: string;
begin
  Result := 0;
  FFullResult.Clear;
  s := FSock.RecvString(FTimeout);
  if Pos('+OK', s) = 1 then
    Result := 1;
  FResultString := s;
  if Full and (Result = 1) then
    repeat
      s := FSock.RecvString(FTimeout);
      if s = '.' then
        Break;
      if s <> '' then
        if s[1] = '.' then
          Delete(s, 1, 1);
      FFullResult.Add(s);
    until FSock.LastError <> 0;
  FResultCode := Result;
end;

function TPOP3Send.AuthLogin: Boolean;
begin
  Result := False;
  FSock.SendString('USER ' + FUserName + CRLF);
  if ReadResult(False) <> 1 then
    Exit;
  FSock.SendString('PASS ' + FPassword + CRLF);
  Result := ReadResult(False) = 1;
end;

function TPOP3Send.AuthAPOP: Boolean;
var
  s: string;
begin
  s := StrToHex(MD5(FTimeStamp + FPassWord));
  FSock.SendString('APOP ' + FUserName + ' ' + s + CRLF);
  Result := ReadResult(False) = 1;
end;

function TPOP3Send.Connect: Boolean;
begin
  // Do not call this function! It is calling by LOGIN method!
  FStatCount := 0;
  FStatSize := 0;
  FSock.CloseSocket;
  FSock.LineBuffer := '';
  FSock.Bind(FIPInterface, cAnyPort);
{$IFDEF STREAMSEC}
  if FFullSSL then
  begin
    if Assigned(FTLSServer) then
      FSock.TLSServer := FTLSServer
    else
    begin
      Result := false;
      Exit;
    end;
  end
  else
    FSock.TLSServer := nil;
{$ELSE}
  if FFullSSL then
    FSock.SSLEnabled := True;
{$ENDIF}
  if FSock.LastError = 0 then
    FSock.Connect(FTargetHost, FTargetPort);
  Result := FSock.LastError = 0;
end;

function TPOP3Send.Capability: Boolean;
begin
  FPOP3cap.Clear;
  FSock.SendString('CAPA' + CRLF);
  Result := ReadResult(True) = 1;
  if Result then
    FPOP3cap.AddStrings(FFullResult);
end;

function TPOP3Send.Login: Boolean;
var
  s, s1: string;
begin
  Result := False;
  FTimeStamp := '';
  if not Connect then
    Exit;
  if ReadResult(False) <> 1 then
    Exit;
  s := SeparateRight(FResultString, '<');
  if s <> FResultString then
  begin
    s1 := Trim(SeparateLeft(s, '>'));
    if s1 <> s then
      FTimeStamp := '<' + s1 + '>';
  end;
  Result := False;
  if Capability then
    if FAutoTLS and (Findcap('STLS') <> '') then
      if StartTLS then
        Capability
      else
      begin
        Result := False;
        Exit;
      end;
  if (FTimeStamp <> '') and not (FAuthType = POP3AuthLogin) then
  begin
    Result := AuthApop;
    if not Result then
    begin
      if not Connect then
        Exit;
      if ReadResult(False) <> 1 then
        Exit;
    end;
  end;
  if not Result and not (FAuthType = POP3AuthAPOP) then
    Result := AuthLogin;
end;

function TPOP3Send.Logout: Boolean;
begin
  FSock.SendString('QUIT' + CRLF);
  Result := ReadResult(False) = 1;
  FSock.CloseSocket;
end;

function TPOP3Send.Reset: Boolean;
begin
  FSock.SendString('RSET' + CRLF);
  Result := ReadResult(False) = 1;
end;

function TPOP3Send.NoOp: Boolean;
begin
  FSock.SendString('NOOP' + CRLF);
  Result := ReadResult(False) = 1;
end;

function TPOP3Send.Stat: Boolean;
var
  s: string;
begin
  Result := False;
  FSock.SendString('STAT' + CRLF);
  if ReadResult(False) <> 1 then
    Exit;
  s := SeparateRight(ResultString, '+OK ');
  FStatCount := StrToIntDef(Trim(SeparateLeft(s, ' ')), 0);
  FStatSize := StrToIntDef(Trim(SeparateRight(s, ' ')), 0);
  Result := True;
end;

function TPOP3Send.List(Value: Integer): Boolean;
begin
  if Value = 0 then
    FSock.SendString('LIST' + CRLF)
  else
    FSock.SendString('LIST ' + IntToStr(Value) + CRLF);
  Result := ReadResult(Value = 0) = 1;
end;

function TPOP3Send.Retr(Value: Integer): Boolean;
begin
  FSock.SendString('RETR ' + IntToStr(Value) + CRLF);
  Result := ReadResult(True) = 1;
end;

function TPOP3Send.Dele(Value: Integer): Boolean;
begin
  FSock.SendString('DELE ' + IntToStr(Value) + CRLF);
  Result := ReadResult(False) = 1;
end;

function TPOP3Send.Top(Value, Maxlines: Integer): Boolean;
begin
  FSock.SendString('TOP ' + IntToStr(Value) + ' ' + IntToStr(Maxlines) + CRLF);
  Result := ReadResult(True) = 1;
end;

function TPOP3Send.Uidl(Value: Integer): Boolean;
begin
  if Value = 0 then
    FSock.SendString('UIDL' + CRLF)
  else
    FSock.SendString('UIDL ' + IntToStr(Value) + CRLF);
  Result := ReadResult(Value = 0) = 1;
end;

function TPOP3Send.StartTLS: Boolean;
begin
  Result := False;
  FSock.SendString('STLS' + CRLF);
  if ReadResult(False) = 1 then
  begin
{$IFDEF STREAMSEC}
    if Assigned(FTLSServer) then
    begin
      Fsock.TLSServer := FTLSServer;
      Fsock.Connect('','');
      Result := FSock.LastError = 0;
    end
    else
      Result := false;
{$ELSE}
    Fsock.SSLDoConnect;
    Result := FSock.LastError = 0;
{$ENDIF}
  end;
end;

function TPOP3Send.FindCap(const Value: string): string;
var
  n: Integer;
  s: string;
begin
  s := UpperCase(Value);
  Result := '';
  for n := 0 to FPOP3cap.Count - 1 do
    if Pos(s, UpperCase(FPOP3cap[n])) = 1 then
    begin
      Result := FPOP3cap[n];
      Break;
    end;
end;

end.
