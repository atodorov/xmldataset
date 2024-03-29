{==============================================================================|
| Project : Ararat Synapse                                       | 008.003.007 |
|==============================================================================|
| Content: Library base                                                        |
|==============================================================================|
| Copyright (c)1999-2004, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)1999-2004.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{
Special thanks to Gregor Ibic <gregor.ibic@intelicom.si>
 (Intelicom d.o.o., http://www.intelicom.si)
 for good inspiration about SSL programming.
}

{$DEFINE ONCEWINSOCK}
{Note about define ONCEWINSOCK:
If you remove this compiler directive, then socket interface is loaded and
initialized on constructor of TBlockSocket class for each socket separately.
Socket interface is used only if your need it.

If you leave this directive here, then socket interface is loaded and
initialized only once at start of your program! It boost performace on high
count of created and destroyed sockets. It eliminate possible small resource
leak on Windows systems too.
}

//{$DEFINE RAISEEXCEPT}
{When you enable this define, then is Raiseexcept property is on by default
}

{:@abstract(Synapse's library core)

Core with implementation basic socket classes.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE BCB}
{$ENDIF}
{$IFDEF BCB}
  {$ObjExportAll On}
{$ENDIF}
{$Q-}
{$H+}
{$M+}

unit blcksock;

interface

uses
  SysUtils, Classes,
{$IFDEF LINUX}
  {$IFDEF FPC}
  synafpc,
  {$ENDIF}
  Libc,
{$ENDIF}
{$IFDEF WIN32}
  Windows,
{$ENDIF}
  synsock, synautil, synacode
{$IFDEF CIL}
  ,System.Net
  ,System.Net.Sockets
  ,System.Text
{$ENDIF}
  , synassl;

const

  SynapseRelease = '35';

  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cBroadcast = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost = '::0';
  c6Broadcast = 'ffff::1';
  cAnyPort = '0';
  CR = #$0d;
  LF = #$0a;
  CRLF = CR + LF;
  c64k = 65535;


type

  {:@abstract(Exception clas used by Synapse)
   When you enable generating of exceptions, this exception is raised by
   Synapse's units.}
  ESynapseError = class(Exception)
  private
    FErrorCode: Integer;
    FErrorMessage: string;
  published
    {:Code of error. Value depending on used operating system}
    property ErrorCode: Integer read FErrorCode Write FErrorCode;
    {:Human readable description of error.}
    property ErrorMessage: string read FErrorMessage Write FErrorMessage;
  end;

  {:Types of OnStatus events}
  THookSocketReason = (
    {:Resolving is begin. Resolved IP and port is in parameter in format like:
     'localhost.somewhere.com:25'.}
    HR_ResolvingBegin,
    {:Resolving is done. Resolved IP and port is in parameter in format like:
     'localhost.somewhere.com:25'. It is always same as in HR_ResolvingBegin!}
    HR_ResolvingEnd,
    {:Socket created by CreateSocket method. It reporting Family of created
     socket too!}
    HR_SocketCreate,
    {:Socket closed by CloseSocket method.}
    HR_SocketClose,
    {:Socket binded to IP and Port. Binded IP and Port is in parameter in format
     like: 'localhost.somewhere.com:25'.}
    HR_Bind,
    {:Socket connected to IP and Port. Connected IP and Port is in parameter in
     format like: 'localhost.somewhere.com:25'.}
    HR_Connect,
    {:Called when CanRead method is used with @True result.}
    HR_CanRead,
    {:Called when CanWrite method is used with @True result.}
    HR_CanWrite,
    {:Socket is swithed to Listen mode. (TCP socket only)}
    HR_Listen,
    {:Socket Accepting client connection. (TCP socket only)}
    HR_Accept,
    {:report count of bytes readed from socket. Number is in parameter string.
     If you need is in integer, you must use StrToInt function!}
    HR_ReadCount,
    {:report count of bytes writed to socket. Number is in parameter string. If
     you need is in integer, you must use StrToInt function!}
    HR_WriteCount,
    {:If is limiting of bandwidth on, then this reason is called when sending or
     receiving is stopped for satisfy bandwidth limit. Parameter is count of
     waiting milliseconds.}
    HR_Wait,
    {:report situation where communication error occured. When raiseexcept is
     @true, then exception is called after this Hook reason.}
    HR_Error
    );

  {:Procedural type for OnStatus event. Sender is calling TBlockSocket object,
   Reason is one of set Status events and value is optional data.}
  THookSocketStatus = procedure(Sender: TObject; Reason: THookSocketReason;
    const Value: string) of object;

  {:this procedural type is used for dataFilter hooks.}
  THookDataFilter = procedure(Sender: TObject; var Value: string) of object;

  {:This procedural type is used for hook OnCreateSocket. By this hook you can
   insert your code after initialisation of socket. (you can set special socket
   options, etc.)}
  THookCreateSocket = procedure(Sender: TObject) of object;

  {:Specify family of socket.}
  TSocketFamily = (
    {:Default mode. Socket family is defined by target address for connection.
     It allows instant access to IPv4 and IPv6 nodes. When you need IPv6 address
     as destination, then is used IPv6 mode. othervise is used IPv4 mode.
     However this mode not working properly with preliminary IPv6 supports!}
    SF_Any,
    {:Turn this class to pure IPv4 mode. This mode is totally compatible with
     previous Synapse releases.}
    SF_IP4,
    {:Turn to only IPv6 mode.}
    SF_IP6
    );

  {:specify possible values of SOCKS modes.}
  TSocksType = (
    ST_Socks5,
    ST_Socks4
    );

  {:Specify requested SSL/TLS version for secure connection.}
  TSSLType = (
    LT_SSLv2,
    LT_SSLv3,
    LT_TLSv1,
    LT_all
    );

  {:Specify type of socket delayed option.}
  TSynaOptionType = (
    SOT_Linger,
    SOT_RecvBuff,
    SOT_SendBuff,
    SOT_NonBlock,
    SOT_RecvTimeout,
    SOT_SendTimeout,
    SOT_Reuse,
    SOT_TTL,
    SOT_Broadcast,
    SOT_MulticastTTL,
    SOT_MulticastLoop
    );

  {:@abstract(this object is used for remember delayed socket option set.)}
  TSynaOption = class(TObject)
  public
    Option: TSynaOptionType;
    Enabled: Boolean;
    Value: Integer;
  end;

  {:@abstract(Basic IP object.)
   This is parent class for other class with protocol implementations. Do not
   use this class directly! Use @link(TICMPBlockSocket), @link(TRAWBlockSocket),
   @link(TTCPBlockSocket) or @link(TUDPBlockSocket) instead.}
  TBlockSocket = class(TObject)
  private
    FOnStatus: THookSocketStatus;
    FOnReadFilter: THookDataFilter;
    FOnWriteFilter: THookDataFilter;
    FOnCreateSocket: THookCreateSocket;
    FLocalSin: TVarSin;
    FRemoteSin: TVarSin;
    FTag: integer;
    FBuffer: string;
    FRaiseExcept: Boolean;
    FNonBlockMode: Boolean;
    FMaxLineLength: Integer;
    FMaxSendBandwidth: Integer;
    FNextSend: ULong;
    FMaxRecvBandwidth: Integer;
    FNextRecv: ULong;
    FConvertLineEnd: Boolean;
    FLastCR: Boolean;
    FLastLF: Boolean;
    FBinded: Boolean;
    FFamily: TSocketFamily;
    FFamilySave: TSocketFamily;
    FIP6used: Boolean;
    FPreferIP4: Boolean;
    FDelayedOptions: TList;
    FInterPacketTimeout: Boolean;
    {$IFNDEF CIL}
    FFDSet: TFDSet;
    {$ENDIF}
    FRecvCounter: Integer;
    FSendCounter: Integer;
    FSendMaxChunk: Integer;
    FStopFlag: Boolean;
    function GetSizeRecvBuffer: Integer;
    procedure SetSizeRecvBuffer(Size: Integer);
    function GetSizeSendBuffer: Integer;
    procedure SetSizeSendBuffer(Size: Integer);
    procedure SetNonBlockMode(Value: Boolean);
    procedure SetTTL(TTL: integer);
    function GetTTL:integer;
    function IsNewApi: Boolean;
    procedure SetFamily(Value: TSocketFamily); virtual;
    procedure SetSocket(Value: TSocket); virtual;
    function GetWsaData: TWSAData;
  protected
    FSocket: TSocket;
    FLastError: Integer;
    FLastErrorDesc: string;
    procedure SetDelayedOption(const Value: TSynaOption);
    procedure DelayedOption(const Value: TSynaOption);
    procedure ProcessDelayedOptions;
    procedure InternalCreateSocket(Sin: TVarSin);
    procedure SetSin(var Sin: TVarSin; IP, Port: string);
    function GetSinIP(Sin: TVarSin): string;
    function GetSinPort(Sin: TVarSin): Integer;
    procedure DoStatus(Reason: THookSocketReason; const Value: string);
    procedure DoReadFilter(Buffer: TMemory; var Len: Integer);
    procedure DoWriteFilter(Buffer: TMemory; var Len: Integer);
    procedure DoCreateSocket;
    procedure LimitBandwidth(Length: Integer; MaxB: integer; var Next: ULong);
    procedure SetBandwidth(Value: Integer);
    function TestStopFlag: Boolean;
  public
    constructor Create;

    {:Create object and load all necessary socket library. What library is
     loaded is described by STUB parameter. If STUB is empty string, then is
     loaded default libraries.}
    constructor CreateAlternate(Stub: string);
    destructor Destroy; override;

    {:If @link(family) is not SF_Any, then create socket with type defined in
     @link(Family) property. If family is SF_Any, then do nothing! (socket is
     created automaticly when you know what type of socket you need to create.
     (i.e. inside @link(Connect) or @link(Bind) call.) When socket is created,
     then is aplyed all stored delayed socket options.}
    procedure CreateSocket;

    {:It create socket. Address resolving of Value tells what type of socket is
     created. If Value is resolved as IPv4 IP, then is created IPv4 socket. If
     value is resolved as IPv6 address, then is created IPv6 socket.}
    procedure CreateSocketByName(const Value: String);

    {:Destroy socket in use. This method is also automatically called from
     object destructor.}
    procedure CloseSocket; virtual;

    {:Abort any work on Socket and destroy them.}
    procedure AbortSocket; virtual;

    {:Connects socket to local IP address and PORT. IP address may be numeric or
     symbolic ('192.168.74.50', 'cosi.nekde.cz', 'ff08::1'). The same for PORT
     - it may be number or mnemonic port ('23', 'telnet').

     If port value is '0', system chooses itself and conects unused port in the
     range 1024 to 4096 (this depending by operating system!). Structure
     LocalSin is filled after calling this method.

     Note: If you call this on non-created socket, then socket is created
     automaticly.

     Warning: when you call : Bind('0.0.0.0','0'); then is nothing done! In this
     case is used implicit system bind instead.}
    procedure Bind(IP, Port: string);

    {:Connects socket to remote IP address and PORT. The same rules as with
     @link(BIND) method are valid. The only exception is that PORT with 0 value
     will not be connected!

     Structures LocalSin and RemoteSin will be filled with valid values.

     When you call this on non-created socket, then socket is created
     automaticly. Type of created socket is by @link(Family) property. If is
     used SF_IP4, then is created socket for IPv4. If is used SF_IP6, then is
     created socket for IPv6. When you have family on SF_Any (default!), then
     type of created socket is determined by address resolving of destination
     address. (Not work properly on prilimitary winsock IPv6 support!)}
    procedure Connect(IP, Port: string); virtual;

    {:Sends data of LENGTH from BUFFER address via connected socket. System
     automatically splits data to packets.}
    function SendBuffer(Buffer: Tmemory; Length: Integer): Integer; virtual;

    {:One data BYTE is sent via connected socket.}
    procedure SendByte(Data: Byte); virtual;

    {:Send data string via connected socket. Any terminator is not added! If you
     need send true string with CR-LF termination, you must add CR-LF characters
     to sended string! Because any termination is not added automaticly, you can
     use this function for sending any binary data in binary string.}
    procedure SendString(Data: AnsiString); virtual;

    {:Send integer as four bytes to socket.}
    procedure SendInteger(Data: integer); virtual;

    {:Send data as one block to socket. Each block begin with 4 bytes with
     length of data in block. This 4 bytes is added automaticly by this
     function.}
    procedure SendBlock(const Data: AnsiString); virtual;

    {:Send data from stream to socket.}
    procedure SendStreamRaw(const Stream: TStream); virtual;

    {:Send content of stream to socket. It using @link(SendBlock) method}
    procedure SendStream(const Stream: TStream); virtual;

    {:Send content of stream to socket. It using @link(SendBlock) method and
    this is compatible with streams in Indy library.}
    procedure SendStreamIndy(const Stream: TStream); virtual;

    {:Note: This is low-level receive function. You must be sure if data is
     waiting for read before call this function for avoid deadlock!

     Waits until allocated buffer is filled by received data. Returns number of
     data received, which equals to LENGTH value under normal operation. If it
     is not equal the communication channel is possibly broken.

     On stream oriented sockets if is received 0 bytes, it mean 'socket is
     closed!"

     On datagram socket is readed first waiting datagram.}
    function RecvBuffer(Buffer: TMemory; Length: Integer): Integer; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions!

     Method waits until data is received. If no data is received within TIMEOUT
     (in milliseconds) period, @link(LastError) is set to WSAETIMEDOUT. Methods
     serves for reading any size of data (i.e. one megabyte...). This method is
     preffered for reading from stream sockets (like TCP).}
    function RecvBufferEx(Buffer: Tmemory; Len: Integer;
      Timeout: Integer): Integer; virtual;

    {:Similar to @link(RecvBufferEx), but readed data is stored in binary
     string, not in memory buffer.}
    function RecvBufferStr(Length: Integer; Timeout: Integer): AnsiString; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Waits until one data byte is received which is also returned as function
     result. If no data is received within TIMEOUT (in milliseconds)period,
     @link(LastError) is set to WSAETIMEDOUT and result have value 0.}
    function RecvByte(Timeout: Integer): Byte; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Waits until one four bytes are received and return it as one Ineger Value.
     If no data is received within TIMEOUT (in milliseconds)period,
     @link(LastError) is set to WSAETIMEDOUT and result have value 0.}
    function RecvInteger(Timeout: Integer): Integer; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Method waits until data string is received. This string is terminated by
     CR-LF characters. The resulting string is returned without this termination
     (CR-LF)! If @link(ConvertLineEnd) is used, then CR-LF sequence may not be
     exactly CR-LF. See @link(ConvertLineEnd) description. If no data is
     received within TIMEOUT (in milliseconds) period, @link(LastError) is set
     to WSAETIMEDOUT. You may also specify maximum length of reading data by
     @link(MaxLineLength) property.}
    function RecvString(Timeout: Integer): AnsiString; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Method waits until data string is received. This string is terminated by
     Terminator string. The resulting string is returned without this
     termination. If no data is received within TIMEOUT (in milliseconds)
     period, @link(LastError) is set to WSAETIMEDOUT. You may also specify
     maximum length of reading data by @link(MaxLineLength) property.}
    function RecvTerminated(Timeout: Integer; const Terminator: AnsiString): AnsiString; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Method reads all data waiting for read. If no data is received within
     TIMEOUT (in milliseconds) period, @link(LastError) is set to WSAETIMEDOUT.
     Methods serves for reading unknown size of data. Because before call this
     function you don't know size of received data, returned data is stored in
     dynamic size binary string. This method is preffered for reading from
     stream sockets (like TCP). It is very goot for receiving datagrams too!
     (UDP protocol)}
    function RecvPacket(Timeout: Integer): AnsiString; virtual;

    {:Read one block of data from socket. Each block begin with 4 bytes with
     length of data in block. This function read first 4 bytes for get lenght,
     then it wait for reported count of bytes.}
    function RecvBlock(Timeout: Integer): AnsiString; virtual;

    {:Read all data from socket to stream until socket is closed (or any error
     occured.)}
    procedure RecvStreamRaw(const Stream: TStream; Timeout: Integer); virtual;
    {:Read requested count of bytes from socket to stream.}
    procedure RecvStreamSize(const Stream: TStream; Timeout: Integer; Size: Integer);

    {:Receive data to stream. It using @link(RecvBlock) method.}
    procedure RecvStream(const Stream: TStream; Timeout: Integer); virtual;

    {:Receive data to stream. This function is compatible with similar function
    in Indy library. It using @link(RecvBlock) method.}
    procedure RecvStreamIndy(const Stream: TStream; Timeout: Integer); virtual;

    {:Same as @link(RecvBuffer), but readed data stays in system input buffer.
    Warning: this function not respect data in @link(LineBuffer)! Is not
    recommended to use this function!}
    function PeekBuffer(Buffer: TMemory; Length: Integer): Integer; virtual;

    {:Same as @link(RecvByte), but readed data stays in input system buffer.
     Warning: this function not respect data in @link(LineBuffer)! Is not
    recommended to use this function!}
    function PeekByte(Timeout: Integer): Byte; virtual;

    {:On stream sockets it returns number of received bytes waiting for picking.
     0 is returned when there is no such data. On datagram socket it returns
     length of the first waiting datagram. Returns 0 if no datagram is waiting.}
    function WaitingData: Integer; virtual;

    {:Same as @link(WaitingData), but if exists some of data in @link(Linebuffer),
     return their length instead.}
    function WaitingDataEx: Integer;

    {:Clear all waiting data for read from buffers.}
    procedure Purge;

    {:Sets linger. Enabled linger means that the system waits another LINGER
     (in milliseconds) time for delivery of sent data. This function is only for
     stream type of socket! (TCP)}
    procedure SetLinger(Enable: Boolean; Linger: Integer);

    {:Actualize values in @link(LocalSin).}
    procedure GetSinLocal;

    {:Actualize values in @link(RemoteSin).}
    procedure GetSinRemote;

    {:Actualize values in @link(LocalSin) and @link(RemoteSin).}
    procedure GetSins;

    {:If you "manually" call Socket API functions, forward their return code as
     parameter to this function, which evaluates it, eventually calls
     GetLastError and found error code returns and stores to @link(LastError).}
    function SockCheck(SockResult: Integer): Integer;

    {:If @link(LastError) contains some error code and @link(RaiseExcept)
     property is @true, raise adequate exception.}
    procedure ExceptCheck;

    {:Returns local computer name as numerical or symbolic value. It try get
     fully qualified domain name. Name is returned in the format acceptable by
     functions demanding IP as input parameter.}
    function LocalName: string;

    {:Try resolve name to all possible IP address. i.e. If you pass as name
     result of @link(LocalName) method, you get all IP addresses used by local
     system.}
    procedure ResolveNameToIP(Name: string; const IPList: TStrings);

    {:Try resolve name to primary IP address. i.e. If you pass as name result of
     @link(LocalName) method, you get primary IP addresses used by local system.}
    function ResolveName(Name: string): string;

    {:Try resolve IP to their primary domain name. If IP not have domain name,
     then is returned original IP.}
    function ResolveIPToName(IP: string): string;

    {:Try resolve symbolic port name to port number. (i.e. 'Echo' to 8)}
    function ResolvePort(Port: string): Word;

    {:Set information about remote side socket. It is good for seting remote
     side for sending UDP packet, etc.}
    procedure SetRemoteSin(IP, Port: string);

    {:Picks IP socket address from @link(LocalSin).}
    function GetLocalSinIP: string; virtual;

    {:Picks IP socket address from @link(RemoteSin).}
    function GetRemoteSinIP: string; virtual;

    {:Picks socket PORT number from @link(LocalSin).}
    function GetLocalSinPort: Integer; virtual;

    {:Picks socket PORT number from @link(RemoteSin).}
    function GetRemoteSinPort: Integer; virtual;

    {:Return @TRUE, if you can read any data from socket or is incoming
     connection on TCP based socket. Status is tested for time Timeout (in
     milliseconds). If value in Timeout is 0, status is only tested and
     continue. If value in Timeout is -1, run is breaked and waiting for read
     data maybe forever.

     This function is need only on special cases, when you need use
     @link(RecvBuffer) function directly!}
    function CanRead(Timeout: Integer): Boolean; virtual;

    {:Same as @link(CanRead), but additionally return @TRUE if is some data in
     @link(LineBuffer).}
    function CanReadEx(Timeout: Integer): Boolean; virtual;

    {:Return @TRUE, if you can to socket write any data (not full sending
     buffer). Status is tested for time Timeout (in milliseconds). If value in
     Timeout is 0, status is only tested and continue. If value in Timeout is
     -1, run is breaked and waiting for write data maybe forever.

     This function is need only on special cases!}
    function CanWrite(Timeout: Integer): Boolean; virtual;

    {:Same as @link(SendBuffer), but send datagram to address from
     @link(RemoteSin). Usefull for sending reply to datagram received by
     function @link(RecvBufferFrom).}
    function SendBufferTo(Buffer: TMemory; Length: Integer): Integer; virtual;

    {:Note: This is low-lever receive function. You must be sure if data is
     waiting for read before call this function for avoid deadlock!

     Receives first waiting datagram to allocated buffer. If there is no waiting
     one, then waits until one comes. Returns length of datagram stored in
     BUFFER. If length exceeds buffer datagram is truncated. After this
     @link(RemoteSin) structure contains information about sender of UDP packet.}
    function RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer; virtual;
{$IFNDEF CIL}
    {:This function is for check for incoming data on set of sockets. Whitch
    sockets is checked is decribed by SocketList Tlist with TBlockSocket
    objects. TList may have maximal number of objects defined by FD_SETSIZE
    constant. Return @TRUE, if you can from some socket read any data or is
    incoming connection on TCP based socket. Status is tested for time Timeout
    (in milliseconds). If value in Timeout is 0, status is only tested and
    continue. If value in Timeout is -1, run is breaked and waiting for read
    data maybe forever. If is returned @TRUE, CanReadList TList is filled by all
    TBlockSocket objects what waiting for read.}
    function GroupCanRead(const SocketList: TList; Timeout: Integer;
      const CanReadList: TList): Boolean;
{$ENDIF}
    {:By this method you may turn address reuse mode for local @link(bind). It
     is good specially for UDP protocol. Using this with TCP protocol is
     hazardous!}
    procedure EnableReuse(Value: Boolean);

    {:Try set timeout for all sending and receiving operations, if socket
     provider can do it. (It not supported by all socket providers!)}
    procedure SetTimeout(Timeout: Integer);

    {:Try set timeout for all sending operations, if socket provider can do it.
     (It not supported by all socket providers!)}
    procedure SetSendTimeout(Timeout: Integer);

    {:Try set timeout for all receiving operations, if socket provider can do
     it. (It not supported by all socket providers!)}
    procedure SetRecvTimeout(Timeout: Integer);

    {:Convert IPv6 address from their string form to binary. This function
     working only on systems with IPv6 support!}
    function StrToIP6(const value: string): TSockAddrIn6;

    {:Convert IPv6 address from binary to string form. This function working
     only on systems with IPv6 support!}
    function IP6ToStr(const value: TSockAddrIn6): string;

    {:Return value of socket type.}
    function GetSocketType: integer; Virtual;

    {:Return value of protocol type for socket creation.}
    function GetSocketProtocol: integer; Virtual;

    {:WSA structure with information about socket provider. On linux is this
     structure simulated!}
    property WSAData: TWSADATA read GetWsaData;

    {:Structure describing local socket side.}
    property LocalSin: TVarSin read FLocalSin write FLocalSin;

    {:Structure describing remote socket side.}
    property RemoteSin: TVarSin read FRemoteSin write FRemoteSin;

    {:Socket handler. Suitable for "manual" calls to socket API or manual
     connection of socket to a previously created socket (i.e by Accept method
     on TCP socket)}
    property Socket: TSocket read FSocket write SetSocket;

    {:Last socket operation error code. Error codes are described in socket
     documentation. Human readable error description is stored in
     @link(LastErrorDesc) property.}
    property LastError: Integer read FLastError;

    {:Human readable error description of @link(LastError) code.}
    property LastErrorDesc: string read FLastErrorDesc;

    {:Buffer used by all high-level receiving functions. This buffer is used for
     optimized reading of data from socket. In normal cases you not need access
     to this buffer directly!}
    property LineBuffer: string read FBuffer write FBuffer;

    {:Size of Winsock receive buffer. If it is not supported by socket provider,
     it return as size one kilobyte.}
    property SizeRecvBuffer: Integer read GetSizeRecvBuffer write SetSizeRecvBuffer;

    {:Size of Winsock send buffer. If it is not supported by socket provider, it
     return as size one kilobyte.}
    property SizeSendBuffer: Integer read GetSizeSendBuffer write SetSizeSendBuffer;

    {:If @True, turn class to non-blocking mode. Not all functions are working
     properly in this mode, you must know exactly what you are doing! However
     when you have big experience with non-blocking programming, then you can
     optimise your program by non-block mode!}
    property NonBlockMode: Boolean read FNonBlockMode Write SetNonBlockMode;

    {:Set Time-to-live value. (if system supporting it!)}
    property TTL: Integer read GetTTL Write SetTTL;

    {:If is @true, then class in in IPv6 mode.}
    property IP6used: Boolean read FIP6used;

    {:Return count of received bytes on this socket from begin of current
     connection.}
    property RecvCounter: Integer read FRecvCounter;

    {:Return count of sended bytes on this socket from begin of current
     connection.}
    property SendCounter: Integer read FSendCounter;
  published
    {:Return descriptive string for given error code. This is class function.
     You may call it without created object!}
    class function GetErrorDesc(ErrorCode: Integer): string;

    {:this value is for free use.}
    property Tag: Integer read FTag write FTag;

    {:If @true, winsock errors raises exception. Otherwise is setted
    @link(LastError) value only and you must check it from your program! Default
    value is @false.}
    property RaiseExcept: Boolean read FRaiseExcept write FRaiseExcept;

    {:Define maximum length in bytes of @link(LineBuffer) for high-level
     receiving functions. If this functions try to read more data then this
     limit, error is returned! If value is 0 (default), no limitation is used.
     This is very good protection for stupid attacks to your server by sending
     lot of data without proper terminator... until all your memory is allocated
     by LineBuffer!

     Note: This maximum length is checked only in functions, what read unknown
     number of bytes! (like @link(RecvString) or @link(RecvTerminated))}
    property MaxLineLength: Integer read FMaxLineLength Write FMaxLineLength;

    {:Define maximal bandwidth for all sending operations in bytes per second.
     If value is 0 (default), bandwidth limitation is not used.}
    property MaxSendBandwidth: Integer read FMaxSendBandwidth Write FMaxSendBandwidth;

    {:Define maximal bandwidth for all receiving operations in bytes per second.
     If value is 0 (default), bandwidth limitation is not used.}
    property MaxRecvBandwidth: Integer read FMaxRecvBandwidth Write FMaxRecvBandwidth;

    {:Define maximal bandwidth for all sending and receiving operations in bytes
     per second. If value is 0 (default), bandwidth limitation is not used.}
    property MaxBandwidth: Integer Write SetBandwidth;

    {:Do a conversion of non-standard line terminators to CRLF. (Off by default)
     If @True, then terminators like sigle CR, single LF or LFCR are converted
     to CRLF internally. This have effect only in @link(RecvString) method!}
    property ConvertLineEnd: Boolean read FConvertLineEnd Write FConvertLineEnd;

    {:Specified Family of this socket. When you are using Windows preliminary
     support for IPv6, then I recommend to set this property!}
    property Family: TSocketFamily read FFamily Write SetFamily;

    {:When resolving of domain name return both IPv4 and IPv6 addresses, then
     specify if is used IPv4 (dafault - @true) or IPv6.}
    property PreferIP4: Boolean read FPreferIP4 Write FPreferIP4;

    {:By dafault (@false) is all timeouts used as timeout between two packets in
     reading operations. If you set this to @true, then Timeouts is for overall
     reading operation!}
    property InterPacketTimeout: Boolean read FInterPacketTimeout Write FInterPacketTimeout;

    {:All sended datas was splitted by this value.}
    property SendMaxChunk: Integer read FSendMaxChunk Write FSendMaxChunk;

    {:By setting this property to @true you can stop any communication. You can
     use this property for soft abort of communication.}
    property StopFlag: Boolean read FStopFlag Write FStopFlag;

    {:This event is called by various reasons. It is good for monitoring socket,
     create gauges for data transfers, etc.}
    property OnStatus: THookSocketStatus read FOnStatus write FOnStatus;

    {:this event is good for some internal thinks about filtering readed datas.
     It is used by telnet client by example.}
    property OnReadFilter: THookDataFilter read FOnReadFilter write FOnReadFilter;

    {:This event is good for some internal thinks about filtering writed datas.}
    property OnWriteFilter: THookDataFilter read FOnWriteFilter write FOnWriteFilter;

    {:This event is called after real socket creation for setting special socket
     options, because you not know when socket is created. (it is depended on
     Ipv4, IPv6 or automatic mode)}
    property OnCreateSocket: THookCreateSocket read FOnCreateSocket write FOnCreateSocket;
  end;

  {:@abstract(Support for SOCKS4 and SOCKS5 proxy)
   Layer with definition all necessary properties and functions for
   implementation SOCKS proxy client. Do not use this class directly.}
  TSocksBlockSocket = class(TBlockSocket)
  protected
    FSocksIP: string;
    FSocksPort: string;
    FSocksTimeout: integer;
    FSocksUsername: string;
    FSocksPassword: string;
    FUsingSocks: Boolean;
    FSocksResolver: Boolean;
    FSocksLastError: integer;
    FSocksResponseIP: string;
    FSocksResponsePort: string;
    FSocksLocalIP: string;
    FSocksLocalPort: string;
    FSocksRemoteIP: string;
    FSocksRemotePort: string;
    FBypassFlag: Boolean;
    FSocksType: TSocksType;
    function SocksCode(IP, Port: string): string;
    function SocksDecode(Value: string): integer;
  public
    constructor Create;

    {:Open connection to SOCKS proxy and if @link(SocksUsername) is set, do
     authorisation to proxy. This is needed only in special cases! (it is called
     internally!)}
    function SocksOpen: Boolean;

    {:Send specified request to SOCKS proxy. This is needed only in special
     cases! (it is called internally!)}
    function SocksRequest(Cmd: Byte; const IP, Port: string): Boolean;

    {:Receive response to previosly sended request. This is needed only in
     special cases! (it is called internally!)}
    function SocksResponse: Boolean;

    {:Is @True when class is using SOCKS proxy.}
    property UsingSocks: Boolean read FUsingSocks;

    {:If SOCKS proxy failed, here is error code returned from SOCKS proxy.}
    property SocksLastError: integer read FSocksLastError;
  published
    {:Address of SOCKS server. If value is empty string, SOCKS support is
     disabled. Assingning any value to this property enable SOCKS mode.
     Warning: You cannot combine this mode with HTTP-tunneling mode!}
    property SocksIP: string read FSocksIP write FSocksIP;

    {:Port of SOCKS server. Default value is '1080'.}
    property SocksPort: string read FSocksPort write FSocksPort;

    {:If you need authorisation on SOCKS server, set username here.}
    property SocksUsername: string read FSocksUsername write FSocksUsername;

    {:If you need authorisation on SOCKS server, set password here.}
    property SocksPassword: string read FSocksPassword write FSocksPassword;

    {:Specify timeout for communicatin with SOCKS server. Default is one minute.}
    property SocksTimeout: integer read FSocksTimeout write FSocksTimeout;

    {:If @True, all symbolic names of target hosts is not translated to IP's
     locally, but resolving is by SOCKS proxy. Default is @True.}
    property SocksResolver: Boolean read FSocksResolver write FSocksResolver;

    {:Specify SOCKS type. By default is used SOCKS5, but you can use SOCKS4 too.
     When you select SOCKS4, then if @link(SOCKSResolver) is enabled, then is
     used SOCKS4a. Othervise is used pure SOCKS4.}
    property SocksType: TSocksType read FSocksType write FSocksType;
  end;

  {:@abstract(Implementation of TCP socket.)
   Supported features: IPv4, IPv6, SSL/TLS  (SSL2, SSL3 and TLS), SOCKS5 proxy
   (outgoing connections and limited incomming), SOCKS4/4a proxy (outgoing
   connections and limited incomming), TCP through HTTP proxy tunnel.}
  TTCPBlockSocket = class(TSocksBlockSocket)
  protected
    FSslEnabled: Boolean;
    FSslBypass: Boolean;
    FSsl: PSSL;
    Fctx: PSSL_CTX;
    FSSLPassword: string;
    FSSLCiphers: string;
    FSSLCertificateFile: string;
    FSSLPrivateKeyFile: string;
    FSSLCertCAFile: string;
    FSSLLastError: integer;
    FSSLLastErrorDesc: string;
    FSSLverifyCert: Boolean;
    FSSLType: TSSLType;
    FHTTPTunnelIP: string;
    FHTTPTunnelPort: string;
    FHTTPTunnel: Boolean;
    FHTTPTunnelRemoteIP: string;
    FHTTPTunnelRemotePort: string;
    FHTTPTunnelUser: string;
    FHTTPTunnelPass: string;
    FHTTPTunnelTimeout: integer;
    procedure SetSslEnabled(Value: Boolean);
    function SetSslKeys: boolean;
    function GetSSLLoaded: Boolean;
    procedure SocksDoConnect(IP, Port: string);
    procedure HTTPTunnelDoConnect(IP, Port: string);
  public
    constructor Create;

    {:See @link(TBlockSocket.CloseSocket)}
    procedure CloseSocket; override;

    {:See @link(TBlockSocket.WaitingData)}
    function WaitingData: Integer; override;

    {:Sets socket to receive mode for new incoming connections. It is necessary
     to use @link(TBlockSocket.BIND) function call before this method to select
     receiving port!

     If you use SOCKS, activate incoming TCP connection by this proxy. (By BIND
     method of SOCKS.)}
    procedure Listen; virtual;

    {:Waits until new incoming connection comes. After it comes a new socket is
     automatically created (socket handler is returned by this function as
     result).

     If you use SOCKS, new socket is not created! In this case is used same
     socket as socket for listening! So, you can accept only one connection in
     SOCKS mode.}
    function Accept: TSocket;

    {:Connects socket to remote IP address and PORT. The same rules as with
     @link(TBlockSocket.BIND) method are valid. The only exception is that PORT
     with 0 value will not be connected. After call to this method
     a communication channel between local and remote socket is created. Local
     socket is assigned automatically if not controlled by previous call to
     @link(TBlockSocket.BIND) method. Structures @link(TBlockSocket.LocalSin)
     and @link(TBlockSocket.RemoteSin) will be filled with valid values.

     If you use SOCKS, activate outgoing TCP connection by SOCKS proxy specified
     in @link(TSocksBlockSocket.SocksIP). (By CONNECT method of SOCKS.)

     If you use HTTP-tunnel mode, activate outgoing TCP connection by HTTP
     tunnel specified in @link(HTTPTunnelIP). (By CONNECT method of HTTP
     protocol.)

     If you additionally use SSL mode, then SSL/TLS session was started.

     Note: If you call this on non-created socket, then socket is created
     automaticly.}
    procedure Connect(IP, Port: string); override;

    {:If you need upgrade existing TCP connection to SSL/TLS mode, then call
     this method. This method switch this class to SSL mode and do SSL/TSL
     handshake.}
    procedure SSLDoConnect;

    {:By this method you can downgrade existing SSL/TLS connection to normal TCP
     connection.}
    procedure SSLDoShutdown;

    {:If you need use this component as SSL/TLS TCP server, then after accepting
     of inbound connection you need start SSL/TLS session by this method. Before
     call this function, you must have assigned all neeeded certificates and
     keys!}
    function SSLAcceptConnection: Boolean;

    {:See @link(TBlockSocket.GetLocalSinIP)}
    function GetLocalSinIP: string; override;

    {:See @link(TBlockSocket.GetRemoteSinIP)}
    function GetRemoteSinIP: string; override;

    {:See @link(TBlockSocket.GetLocalSinPort)}
    function GetLocalSinPort: Integer; override;

    {:See @link(TBlockSocket.GetRemoteSinPort)}
    function GetRemoteSinPort: Integer; override;

    {:See @link(TBlockSocket.SendBuffer)}
    function SendBuffer(Buffer: TMemory; Length: Integer): Integer; override;

    {:See @link(TBlockSocket.RecvBuffer)}
    function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; override;

    {:Return string with identificator of SSL/TLS version of existing
     connection.}
    function SSLGetSSLVersion: string;

    {:Return subject of remote SSL peer.}
    function SSLGetPeerSubject: string;

    {:Return issuer certificate of remote SSL peer.}
    function SSLGetPeerIssuer: string;

    {:Return peer name from remote side certificate. This is good for verify,
     if certificate is generated for remote side IP name.}
    function SSLGetPeerName: string;

    {:Return subject's hash of remote SSL peer.}
    function SSLGetPeerSubjectHash: Cardinal;

    {:Return issuer's certificate hash of remote SSL peer.}
    function SSLGetPeerIssuerHash: Cardinal;

    {:Return fingerprint of remote SSL peer.}
    function SSLGetPeerFingerprint: string;

    {:Return all detailed information about certificate from remote side of
     SSL/TLS connection. Result string is multilined!}
    function SSLGetCertInfo: string;

    {:Return currently used Cipher.}
    function SSLGetCipherName: string;

    {:Return currently used number of bits in current Cipher algorythm.}
    function SSLGetCipherBits: integer;

    {:Return number of bits in current Cipher algorythm.}
    function SSLGetCipherAlgBits: integer;

    {:Return result value of verify remote side certificate. Look to OpenSSL
     documentation for possible values. For example 0 is successfuly verified
     certificate, or 18 is self-signed certificate.}
    function SSLGetVerifyCert: integer;

    {:Test last SSL operation for errors. If error occured, then is filled
     @link(SSLLastError) and @link(SSLLastErrorDesc) properties.}
    function SSLCheck: Boolean;

    {:Return value of socket type. For TCP return SOCK_STREAM.}
    function GetSocketType: integer; override;

    {:Return value of protocol type for socket creation. For TCP return
     IPPROTO_TCP.}
    function GetSocketProtocol: integer; override;

    {:Is SSL interface loaded or not?}
    property SSLLoaded: Boolean read GetSslLoaded;

    {:By this property you can enable or disable SSL mode. Enabling loads needed
     OpenSSL or SSLeay libraries. Libraries is loaded to memory only once for
     all Synapse's objects.

     Note: when you enable SSL mode, all keys and certificates are loaded (if
     needed property is unempty)}
    property SSLEnabled: Boolean read FSslEnabled write SetSslEnabled;

    {:Contains last SSL error code.}
    property SSLLastError: integer read FSSLLastError;

    {:If some SSL error is occured, then contains human readable description of
     this error.}
    property SSLLastErrorDesc: string read FSSLLastErrorDesc;

    {:@True if is used HTTP tunnel mode.}
    property HTTPTunnel: Boolean read FHTTPTunnel;
  published
    {:Here you can specify requested SSL/TLS mode. Default is autodetection, but
     on some servers autodetection not working properly. In this case you must
     specify requested SSL/TLS mode by your hand!}
    property SSLType: TSSLType read FSSLType write FSSLType;

    {:If is SSL mode enabled and this property is @TRUE, then all data (read
     and write) will not be encrypted/decrypted.}
    property SSLBypass: Boolean read FSslBypass write FSslBypass;

    {:Password for decrypting of encoded certificate.

     Note: This not work with delphi8. You cannot use password protected
     certificates with .NET!}
    property SSLPassword: string read FSSLPassword write FSSLPassword;

    {:By this property you can modify default set of SSL/TLS ciphers.}
    property SSLCiphers: string read FSSLCiphers write FSSLCiphers;

    {:Filename and path to PEM file with your certificate. If certificate need
     password for decrypt, you can assign this password to SSLPassword property.}
    property SSLCertificateFile: string read FSSLCertificateFile write FSSLCertificateFile;

    {:Filename and path to PEM file with your private key.}
    property SSLPrivateKeyFile: string read FSSLPrivateKeyFile write FSSLPrivateKeyFile;

    {:filename and path to file with bundle of CA certificates. (you may use
     ca-bundle.crt file from SynaCert.zip)}
    property SSLCertCAFile: string read FSSLCertCAFile write FSSLCertCAFile;

    {:If @true, then is verified client certificate. (it is good for writing
     SSL/TLS servers.) When you are not server, but you are client, then if this
     property is @true, verify servers certificate.}
    property SSLverifyCert: Boolean read FSSLverifyCert write FSSLverifyCert;

    {:Specify IP address of HTTP proxy. Assingning non-empty value to this
     property enable HTTP-tunnel mode. This mode is for tunnelling any outgoing
     TCP connection through HTTP proxy server. (If policy on HTTP proxy server
     allow this!) Warning: You cannot combine this mode with SOCK5 mode!}
    property HTTPTunnelIP: string read FHTTPTunnelIP Write FHTTPTunnelIP;

    {:Specify port of HTTP proxy for HTTP-tunneling.}
    property HTTPTunnelPort: string read FHTTPTunnelPort Write FHTTPTunnelPort;

    {:Specify authorisation username for access to HTTP proxy in HTTP-tunnel
     mode. If you not need authorisation, then let this property empty.}
    property HTTPTunnelUser: string read FHTTPTunnelUser Write FHTTPTunnelUser;

    {:Specify authorisation password for access to HTTP proxy in HTTP-tunnel
     mode.}
    property HTTPTunnelPass: string read FHTTPTunnelPass Write FHTTPTunnelPass;

    {:Specify timeout for communication with HTTP proxy in HTTPtunnel mode.}
    property HTTPTunnelTimeout: integer read FHTTPTunnelTimeout Write FHTTPTunnelTimeout;
  end;

  {:@abstract(Datagram based communication)
   This class implementing datagram based communication instead default stream
   based communication style.}
  TDgramBlockSocket = class(TSocksBlockSocket)
  public
    {:Fill @link(TBlockSocket.RemoteSin) structure. This address is used for
     sending data.}
    procedure Connect(IP, Port: string); override;

    {:Silently redirected to @link(TBlockSocket.SendBufferTo).}
    function SendBuffer(Buffer: TMemory; Length: Integer): Integer; override;

    {:Silently redirected to @link(TBlockSocket.RecvBufferFrom).}
    function RecvBuffer(Buffer: TMemory; Length: Integer): Integer; override;
  end;

  {:@abstract(Implementation of UDP socket.)
   NOTE: in this class is all receiving redirected to RecvBufferFrom. You can
   use for reading any receive function. Preffered is RecvPacket! Similary all
   sending is redirected to SendbufferTo. You can use for sending UDP packet any
   sending function, like SendString.

   Supported features: IPv4, IPv6, unicasts, broadcasts, multicasts, SOCKS5
   proxy (only unicasts! Outgoing and incomming.)}
  TUDPBlockSocket = class(TDgramBlockSocket)
  protected
    FSocksControlSock: TTCPBlockSocket;
    function UdpAssociation: Boolean;
    procedure SetMulticastTTL(TTL: integer);
    function GetMulticastTTL:integer;
  public
    destructor Destroy; override;

    {:Enable or disable sending of broadcasts. If seting OK, result is @true.
     This method is not supported in SOCKS5 mode! IPv6 does not support
     broadcasts! In this case you must use Multicasts instead.}
    procedure EnableBroadcast(Value: Boolean);

    {:See @link(TBlockSocket.SendBufferTo)}
    function SendBufferTo(Buffer: TMemory; Length: Integer): Integer; override;

    {:See @link(TBlockSocket.RecvBufferFrom)}
    function RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer; override;
{$IFNDEF CIL}
    {:Add this socket to given multicast group. You cannot use Multicasts in
     SOCKS mode!}
    procedure AddMulticast(MCastIP:string);

    {:Remove this socket from given multicast group.}
    procedure DropMulticast(MCastIP:string);
{$ENDIF}
    {:All sended multicast datagrams is loopbacked to your interface too. (you
     can read your sended datas.) You can disable this feature by this function.
     This function not working on some Windows systems!}
    procedure EnableMulticastLoop(Value: Boolean);

    {:Return value of socket type. For UDP return SOCK_DGRAM.}
    function GetSocketType: integer; override;

    {:Return value of protocol type for socket creation. For UDP return
     IPPROTO_UDP.}
    function GetSocketProtocol: integer; override;

    {:Set Time-to-live value for multicasts packets. It define number of routers
     for transfer of datas. If you set this to 1 (dafault system value), then
     multicasts packet goes only to you local network. If you need transport
     multicast packet to worldwide, then increase this value, but be carefull,
     lot of routers on internet does not transport multicasts packets!}
    property MulticastTTL: Integer read GetMulticastTTL Write SetMulticastTTL;
  end;

  {:@abstract(Implementation of RAW ICMP socket.)
   For this object you must have rights for creating RAW sockets!}
  TICMPBlockSocket = class(TDgramBlockSocket)
  public
    {:Return value of socket type. For RAW and ICMP return SOCK_RAW.}
    function GetSocketType: integer; override;

    {:Return value of protocol type for socket creation. For ICMP returns
     IPPROTO_ICMP or IPPROTO_ICMPV6}
    function GetSocketProtocol: integer; override;
  end;

  {:@abstract(Implementation of RAW socket.)
   For this object you must have rights for creating RAW sockets!}
  TRAWBlockSocket = class(TBlockSocket)
  public
    {:Return value of socket type. For RAW and ICMP return SOCK_RAW.}
    function GetSocketType: integer; override;

    {:Return value of protocol type for socket creation. For RAW returns
     IPPROTO_RAW.}
    function GetSocketProtocol: integer; override;
  end;

  {:@abstract(Record with definition of IP packet header.)
   For reading data from ICMP or RAW sockets.}
  TIPHeader = record
    VerLen: Byte;
    TOS: Byte;
    TotalLen: Word;
    Identifer: Word;
    FragOffsets: Word;
    TTL: Byte;
    Protocol: Byte;
    CheckSum: Word;
    SourceIp: DWORD;
    DestIp: DWORD;
    Options: DWORD;
  end;

  {:@abstract(Parent class of application protocol implementations.)
   By this class is defined common properties.}
  TSynaClient = Class(TObject)
  protected
    FTargetHost: string;
    FTargetPort: string;
    FIPInterface: string;
    FTimeout: integer;
    FUserName: string;
    FPassword: string;
  public
    constructor Create;
  published
    {:Specify terget server IP (or symbolic name). Default is 'localhost'.}
    property TargetHost: string read FTargetHost Write FTargetHost;

    {:Specify terget server port (or symbolic name).}
    property TargetPort: string read FTargetPort Write FTargetPort;

    {:Defined local socket address. (outgoing IP address). By default is used
     '0.0.0.0' as wildcard for default IP.}
    property IPInterface: string read FIPInterface Write FIPInterface;

    {:Specify default timeout for socket operations.}
    property Timeout: integer read FTimeout Write FTimeout;

    {:If protocol need user authorization, then fill here username.}
    property UserName: string read FUserName Write FUserName;

    {:If protocol need user authorization, then fill here password.}
    property Password: string read FPassword Write FPassword;
  end;

implementation

{$IFDEF ONCEWINSOCK}
var
  WsaDataOnce: TWSADATA;
  e: ESynapseError;
{$ENDIF}


constructor TBlockSocket.Create;
begin
  CreateAlternate('');
end;

constructor TBlockSocket.CreateAlternate(Stub: string);
{$IFNDEF ONCEWINSOCK}
var
  e: ESynapseError;
{$ENDIF}
begin
  inherited Create;
  FDelayedOptions := TList.Create;
  FRaiseExcept := False;
{$IFDEF RAISEEXCEPT}
  FRaiseExcept := True;
{$ENDIF}
  FSocket := INVALID_SOCKET;
  FBuffer := '';
  FLastCR := False;
  FLastLF := False;
  FBinded := False;
  FNonBlockMode := False;
  FMaxLineLength := 0;
  FMaxSendBandwidth := 0;
  FNextSend := 0;
  FMaxRecvBandwidth := 0;
  FNextRecv := 0;
  FConvertLineEnd := False;
  FFamily := SF_Any;
  FFamilySave := SF_Any;
  FIP6used := False;
  FPreferIP4 := True;
  FInterPacketTimeout := True;
  FRecvCounter := 0;
  FSendCounter := 0;
  FSendMaxChunk := c64k;
  FStopFlag := False;
{$IFNDEF ONCEWINSOCK}
  if Stub = '' then
    Stub := DLLStackName;
  if not InitSocketInterface(Stub) then
  begin
    e := ESynapseError.Create('Error loading Socket interface (' + Stub + ')!');
    e.ErrorCode := 0;
    e.ErrorMessage := 'Error loading Socket interface (' + Stub + ')!';
    raise e;
  end;
  SockCheck(synsock.WSAStartup(WinsockLevel, FWsaDataOnce));
  ExceptCheck;
{$ENDIF}
end;

destructor TBlockSocket.Destroy;
var
  n: integer;
  p: TSynaOption;
begin
  CloseSocket;
{$IFNDEF ONCEWINSOCK}
  synsock.WSACleanup;
  DestroySocketInterface;
{$ENDIF}
  for n := FDelayedOptions.Count - 1 downto 0 do
    begin
      p := TSynaOption(FDelayedOptions[n]);
      p.Free;
    end;
  FDelayedOptions.Free;
  inherited Destroy;
end;

function TBlockSocket.IsNewApi: Boolean;
begin
  Result := SockEnhancedApi;
  if not Result then
    Result := (FFamily = SF_ip6) and SockWship6Api;
end;

procedure TBlockSocket.SetDelayedOption(const Value: TSynaOption);
var
  li: TLinger;
  x: integer;
  buf: TMemory;
begin
  case value.Option of
    SOT_Linger:
      begin
        {$IFDEF CIL}
        li := TLinger.Create(Value.Enabled, Value.Value div 1000);
        synsock.SetSockOptObj(FSocket, integer(SOL_SOCKET), integer(SO_LINGER), li);
        {$ELSE}
        li.l_onoff := Ord(Value.Enabled);
        li.l_linger := Value.Value div 1000;
        buf := @li;
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_LINGER), buf, SizeOf(li));
        {$ENDIF}
      end;
    SOT_RecvBuff:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
        buf := @Value.Value;
        {$ENDIF}
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVBUF),
          buf, SizeOf(Value.Value));
      end;
    SOT_SendBuff:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
        buf := @Value.Value;
        {$ENDIF}
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_SNDBUF),
          buf, SizeOf(Value.Value));
      end;
    SOT_NonBlock:
      begin
        FNonBlockMode := Value.Enabled;
        x := Ord(FNonBlockMode);
        synsock.IoctlSocket(FSocket, FIONBIO, u_long(x));
      end;
    SOT_RecvTimeout:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
        buf := @Value.Value;
        {$ENDIF}
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVTIMEO),
          buf, SizeOf(Value.Value));
      end;
    SOT_SendTimeout:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
        buf := @Value.Value;
        {$ENDIF}
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_SNDTIMEO),
          buf, SizeOf(Value.Value));
      end;
    SOT_Reuse:
      begin
        x := Ord(Value.Enabled);
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(x);
        {$ELSE}
        buf := @x;
        {$ENDIF}
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_REUSEADDR), buf, SizeOf(x));
      end;
    SOT_TTL:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
        buf := @Value.Value;
        {$ENDIF}
        if FIP6Used then
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IPV6), integer(IPV6_UNICAST_HOPS),
            buf, SizeOf(Value.Value))
        else
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IP), integer(IP_TTL),
            buf, SizeOf(Value.Value));
      end;
    SOT_Broadcast:
      begin
//#todo1 broadcasty na IP6
        x := Ord(Value.Enabled);
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(x);
        {$ELSE}
        buf := @x;
        {$ENDIF}
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_BROADCAST), buf, SizeOf(x));
      end;
    SOT_MulticastTTL:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
        buf := @Value.Value;
        {$ENDIF}
        if FIP6Used then
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IPV6), integer(IPV6_MULTICAST_HOPS),
            buf, SizeOf(Value.Value))
        else
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IP), integer(IP_MULTICAST_TTL),
            buf, SizeOf(Value.Value));
      end;
   SOT_MulticastLoop:
      begin
        x := Ord(Value.Enabled);
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(x);
        {$ELSE}
        buf := @x;
        {$ENDIF}
        if FIP6Used then
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IPV6), integer(IPV6_MULTICAST_LOOP), buf, SizeOf(x))
        else
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IP), integer(IP_MULTICAST_LOOP), buf, SizeOf(x));
      end;
  end;
  Value.free;
end;

procedure TBlockSocket.DelayedOption(const Value: TSynaOption);
begin
  if FSocket = INVALID_SOCKET then
  begin
    FDelayedOptions.Insert(0, Value);
  end
  else
    SetDelayedOption(Value);
end;

procedure TBlockSocket.ProcessDelayedOptions;
var
  n: integer;
  d: TSynaOption;
begin
  for n := FDelayedOptions.Count - 1 downto 0 do
  begin
    d := TSynaOption(FDelayedOptions[n]);
    SetDelayedOption(d);
  end;
  FDelayedOptions.Clear;
end;

procedure TBlockSocket.SetSin(var Sin: TVarSin; IP, Port: string);
{$IFNDEF CIL}
type
  pu_long = ^u_long;
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  HostEnt: PHostEnt;
  r: integer;
  Hints1, Hints2: TAddrInfo;
  Sin1, Sin2: TVarSin;
  TwoPass: boolean;

  function GetAddr(const IP, port: string; Hints: TAddrInfo; var Sin: TVarSin): integer;
  var
    Addr: PAddrInfo;
  begin
    Addr := nil;
    try
      FillChar(Sin, Sizeof(Sin), 0);
      if Hints.ai_socktype = SOCK_RAW then
      begin
        Hints.ai_socktype := 0;
        Hints.ai_protocol := 0;
        Result := synsock.GetAddrInfo(PChar(IP), nil, @Hints, Addr);
      end
      else
      begin
        if (IP = cAnyHost) or (IP = c6AnyHost) then
        begin
          Hints.ai_flags := AI_PASSIVE;
          Result := synsock.GetAddrInfo(nil, PChar(Port), @Hints, Addr);
        end
        else
          if (IP = cLocalhost) or (IP = c6Localhost) then
          begin
            Result := synsock.GetAddrInfo(nil, PChar(Port), @Hints, Addr);
          end
          else
          begin
            Result := synsock.GetAddrInfo(PChar(IP), PChar(Port), @Hints, Addr);
          end;
      end;
      if Result = 0 then
        if (Addr <> nil) then
          Move(Addr^.ai_addr^, Sin, Addr^.ai_addrlen);
    finally
      if Assigned(Addr) then
        synsock.FreeAddrInfo(Addr);
    end;
  end;

begin
  DoStatus(HR_ResolvingBegin, IP + ':' + Port);
  FLastError := 0;
  FillChar(Sin, Sizeof(Sin), 0);
  if not IsNewApi then
  begin
    SynSockCS.Enter;
    try
      Sin.sin_family := AF_INET;
      ProtoEnt := synsock.GetProtoByNumber(GetSocketProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
        ServEnt := synsock.GetServByName(PChar(Port), ProtoEnt^.p_name);
      if ServEnt = nil then
        Sin.sin_port := synsock.htons(StrToIntDef(Port, 0))
      else
        Sin.sin_port := ServEnt^.s_port;
      if IP = cBroadcast then
        Sin.sin_addr.s_addr := u_long(INADDR_BROADCAST)
      else
      begin
        Sin.sin_addr.s_addr := synsock.inet_addr(PChar(IP));
        if Sin.sin_addr.s_addr = u_long(INADDR_NONE) then
        begin
          HostEnt := synsock.GetHostByName(PChar(IP));
          FLastError := synsock.WSAGetLastError;
          if HostEnt <> nil then
            Sin.sin_addr.S_addr := u_long(Pu_long(HostEnt^.h_addr_list^)^);
        end;
      end;
    finally
      SynSockCS.Leave;
    end;
  end
  else
  begin
    FillChar(Hints1, Sizeof(Hints1), 0);
    FillChar(Hints2, Sizeof(Hints2), 0);
    TwoPass := False;
    //if socket exists, then use their type, else use users selection
    if FSocket = INVALID_SOCKET then
      case FFamily of
        SF_Any:
          begin
            if IsIP(IP) then
              Hints1.ai_family := AF_INET
            else
              if IsIP6(IP) then
                Hints1.ai_family := AF_INET6
              else
                if FPreferIP4 then
                begin
                  Hints1.ai_family := AF_INET;
                  Hints2.ai_family := AF_INET6;
                  TwoPass := True;
                end
                else
                begin
                  Hints2.ai_family := AF_INET;
                  Hints1.ai_family := AF_INET6;
                  TwoPass := True;
                end
          end;
        SF_IP4:
          Hints1.ai_family := AF_INET;
        SF_IP6:
          Hints1.ai_family := AF_INET6;
      end
    else
      if FIP6Used then
        Hints1.ai_family := AF_INET6
      else
        Hints1.ai_family := AF_INET;
    Hints1.ai_socktype := GetSocketType;
    Hints1.ai_protocol := GetSocketprotocol;
    Hints2.ai_socktype := Hints1.ai_socktype;
    Hints2.ai_protocol := Hints1.ai_protocol;

    r := GetAddr(IP, Port, Hints1, Sin1);
    FLastError := r;
    sin := sin1;
    if r <> 0 then
      if TwoPass then
      begin
        r := GetAddr(IP, Port, Hints2, Sin2);
        FLastError := r;
        if r = 0 then
          sin := sin2;
      end;
  end;
{$ELSE}
var
  IPs: array of IPAddress;
  n: integer;
  ip4, ip6: string;
  sip: string;
begin
  ip4 := '';
  ip6 := '';
  IPs := Dns.Resolve(IP).AddressList;
  for n :=low(IPs) to high(IPs) do begin
    if (ip4 = '') and (IPs[n].AddressFamily = AF_INET) then
      ip4 := IPs[n].toString;
    if (ip6 = '') and (IPs[n].AddressFamily = AF_INET6) then
      ip6 := IPs[n].toString;
    if (ip4 <> '') and (ip6 <> '') then
      break;
  end;
  if FSocket = INVALID_SOCKET then
  case FFamily of
    SF_Any:
      begin
        if (ip4 <> '') and (ip6 <> '') then
        begin
          if FPreferIP4 then
            sip := ip4
          else
            Sip := ip6;
          end
        else
        begin
          sip := ip4;
          if (ip6 <> '') then
            sip := ip6;
        end;
      end;
    SF_IP4:
      sip := ip4;
    SF_IP6:
      sip := ip6;
  end
  else
    if FIP6Used then
      sip := ip6
    else
      sip := ip4;

  sin := TVarSin.Create(IPAddress.Parse(sip), GetPortService(Port));
{$ENDIF}
  DoStatus(HR_ResolvingEnd, IP + ':' + Port);
end;

function TBlockSocket.GetSinIP(Sin: TVarSin): string;
{$IFNDEF CIL}
var
  p: PChar;
  host, serv: string;
  hostlen, servlen: integer;
  r: integer;
begin
  Result := '';
  if not IsNewApi then
  begin
    p := synsock.inet_ntoa(Sin.sin_addr);
    if p <> nil then
      Result := p;
  end
  else
  begin
    hostlen := NI_MAXHOST;
    servlen := NI_MAXSERV;
    setlength(host, hostlen);
    setlength(serv, servlen);
    r := getnameinfo(@sin, SizeOfVarSin(sin), PChar(host), hostlen,
      PChar(serv), servlen, NI_NUMERICHOST + NI_NUMERICSERV);
    if r = 0 then
      Result := PChar(host);
  end;
{$ELSE}
begin
  Result := Sin.Address.ToString;
{$ENDIF}
end;

function TBlockSocket.GetSinPort(Sin: TVarSin): Integer;
{$IFNDEF CIL}
begin
  if (Sin.sin_family = AF_INET6) then
    Result := synsock.ntohs(Sin.sin6_port)
  else
    Result := synsock.ntohs(Sin.sin_port);
{$ELSE}
begin
  Result := Sin.Port;
{$ENDIF}
end;

procedure TBlockSocket.CreateSocket;
var
  sin: TVarSin;
begin
  //dummy for SF_Any Family mode
  FLastError := 0;
  if (FFamily <> SF_Any) and (FSocket = INVALID_SOCKET) then
  begin
    {$IFDEF CIL}
    if FFamily = SF_IP6 then
      sin := TVarSin.Create(IPAddress.Parse('::0'), 0)
    else
      sin := TVarSin.Create(IPAddress.Parse('0.0.0.0'), 0);
    {$ELSE}
    FillChar(Sin, Sizeof(Sin), 0);
    if FFamily = SF_IP6 then
      sin.sin_family := AF_INET6
    else
      sin.sin_family := AF_INET;
    {$ENDIF}
    InternalCreateSocket(Sin);
  end;
end;

procedure TBlockSocket.CreateSocketByName(const Value: String);
var
  sin: TVarSin;
begin
  FLastError := 0;
  if FSocket = INVALID_SOCKET then
  begin
    SetSin(sin, value, '0');
    if FLastError = 0 then
      InternalCreateSocket(Sin);
  end;
end;

procedure TBlockSocket.InternalCreateSocket(Sin: TVarSin);
begin
  FStopFlag := False;
  FRecvCounter := 0;
  FSendCounter := 0;
  FLastError := 0;
  if FSocket = INVALID_SOCKET then
  begin
    FBuffer := '';
    FBinded := False;
    FIP6Used := Sin.AddressFamily = AF_INET6;
    FSocket := synsock.Socket(integer(Sin.AddressFamily), GetSocketType, GetSocketProtocol);
    if FSocket = INVALID_SOCKET then
      FLastError := synsock.WSAGetLastError;
    {$IFNDEF CIL}
    FD_ZERO(FFDSet);
    FD_SET(FSocket, FFDSet);
    {$ENDIF}
    ExceptCheck;
    if FIP6used then
      DoStatus(HR_SocketCreate, 'IPv6')
    else
      DoStatus(HR_SocketCreate, 'IPv4');
    ProcessDelayedOptions;
    DoCreateSocket;
  end;
end;

procedure TBlockSocket.CloseSocket;
begin
  AbortSocket;
end;

procedure TBlockSocket.AbortSocket;
var
  n: integer;
  p: TSynaOption;
begin
  if FSocket <> INVALID_SOCKET then
    synsock.CloseSocket(FSocket);
  FSocket := INVALID_SOCKET;
  for n := FDelayedOptions.Count - 1 downto 0 do
    begin
      p := TSynaOption(FDelayedOptions[n]);
      p.Free;
    end;
  FDelayedOptions.Clear;
  FFamily := FFamilySave;
  FLastError := 0;
  DoStatus(HR_SocketClose, '');
end;

procedure TBlockSocket.Bind(IP, Port: string);
var
  Sin: TVarSin;
begin
  FLastError := 0;
  if (FSocket <> INVALID_SOCKET)
    or not((FFamily = SF_ANY) and (IP = cAnyHost) and (Port = cAnyPort)) then
  begin
    SetSin(Sin, IP, Port);
    if FLastError = 0 then
    begin
      if FSocket = INVALID_SOCKET then
        InternalCreateSocket(Sin);
      SockCheck(synsock.Bind(FSocket, Sin));
      GetSinLocal;
      FBuffer := '';
      FBinded := True;
    end;
    ExceptCheck;
    DoStatus(HR_Bind, IP + ':' + Port);
  end;
end;

procedure TBlockSocket.Connect(IP, Port: string);
var
  Sin: TVarSin;
begin
  SetSin(Sin, IP, Port);
  if FLastError = 0 then
  begin
    if FSocket = INVALID_SOCKET then
      InternalCreateSocket(Sin);
    SockCheck(synsock.Connect(FSocket, Sin));
    if FLastError = 0 then
      GetSins;
    FBuffer := '';
    FLastCR := False;
    FLastLF := False;
  end;
  ExceptCheck;
  DoStatus(HR_Connect, IP + ':' + Port);
end;

procedure TBlockSocket.GetSinLocal;
begin
  synsock.GetSockName(FSocket, FLocalSin);
end;

procedure TBlockSocket.GetSinRemote;
begin
  synsock.GetPeerName(FSocket, FRemoteSin);
end;

procedure TBlockSocket.GetSins;
begin
  GetSinLocal;
  GetSinRemote;
end;

procedure TBlockSocket.SetBandwidth(Value: Integer);
begin
  MaxSendBandwidth := Value;
  MaxRecvBandwidth := Value;
end;

procedure TBlockSocket.LimitBandwidth(Length: Integer; MaxB: integer; var Next: ULong);
var
  x: ULong;
  y: ULong;
begin
  if MaxB > 0 then
  begin
    y := GetTick;
    if Next > y then
    begin
      x := Next - y;
      if x > 0 then
      begin
        DoStatus(HR_Wait, IntToStr(x));
        sleep(x);
      end;
    end;
    Next := GetTick + Trunc((Length / MaxB) * 1000);
  end;
end;

function TBlockSocket.TestStopFlag: Boolean;
begin
  Result := FStopFlag;
  if Result then
  begin
    FStopFlag := False;
    FLastError := WSAECONNABORTED;
    FLastErrorDesc := GetErrorDesc(FLastError);
    ExceptCheck;
  end;
end;


function TBlockSocket.SendBuffer(Buffer: TMemory; Length: Integer): Integer;
{$IFNDEF CIL}
var
  x, y: integer;
  l, r: integer;
  p: Pointer;
{$ENDIF}
begin
  Result := 0;
  if TestStopFlag then
    Exit;
  DoWriteFilter(Buffer, Length);
{$IFDEF CIL}
  Result := synsock.Send(FSocket, Buffer, Length, 0);
{$ELSE}
  l := Length;
  x := 0;
  while x < l do
  begin
    y := l - x;
    if y > FSendMaxChunk then
      y := FSendMaxChunk;
    if y > 0 then
    begin
      LimitBandwidth(y, FMaxSendBandwidth, FNextsend);
      p := IncPoint(Buffer, x);
//      r := synsock.Send(FSocket, p^, y, MSG_NOSIGNAL);
      r := synsock.Send(FSocket, p, y, MSG_NOSIGNAL);
      SockCheck(r);
      if Flasterror <> 0 then
        Break;
      Inc(x, r);
      Inc(Result, r);
      Inc(FSendCounter, r);
      DoStatus(HR_WriteCount, IntToStr(r));
    end
    else
      break;
  end;
{$ENDIF}
  ExceptCheck;
end;

procedure TBlockSocket.SendByte(Data: Byte);
{$IFDEF CIL}
var
  buf: TMemory;
{$ENDIF}
begin
{$IFDEF CIL}
  setlength(buf, 1);
  buf[0] := Data;
  SendBuffer(buf, 1);
{$ELSE}
  SendBuffer(@Data, 1);
{$ENDIF}
end;

procedure TBlockSocket.SendString(Data: AnsiString);
var
  buf: TMemory;
begin
//  SendBuffer(PChar(Data), Length(Data));
  {$IFDEF CIL}
  buf := BytesOf(Data);
  {$ELSE}
  buf := pchar(data);
  {$ENDIF}
  SendBuffer(buf, Length(Data));
end;

procedure TBlockSocket.SendInteger(Data: integer);
var
  buf: TMemory;
begin
  {$IFDEF CIL}
  buf := System.BitConverter.GetBytes(Data);
  {$ELSE}
  buf := @Data;
  {$ENDIF}
  SendBuffer(buf, SizeOf(Data));
end;

procedure TBlockSocket.SendBlock(const Data: AnsiString);
begin
  SendInteger(Length(data));
  SendString(Data);
end;

procedure TBlockSocket.SendStreamRaw(const Stream: TStream);
var
  si: integer;
  x, y, yr: integer;
  s: AnsiString;
{$IFDEF CIL}
  buf: TMemory;
{$ENDIF}
begin
  si := Stream.Size - Stream.Position;
  x := 0;
  while x < si do
  begin
    y := si - x;
    if y > FSendMaxChunk then
      y := FSendMaxChunk;
    {$IFDEF CIL}
    Setlength(buf, y);
    yr := Stream.read(buf, y);
    if yr > 0 then
    begin
      SendBuffer(buf, yr);
      Inc(x, yr);
    end
    else
      break;
    {$ELSE}
    Setlength(s, y);
    yr := Stream.read(Pchar(s)^, y);
    if yr > 0 then
    begin
      SetLength(s, yr);
      SendString(s);
      Inc(x, yr);
    end
    else
      break;
    {$ENDIF}
  end;
end;

procedure TBlockSocket.SendStreamIndy(const Stream: TStream);
var
  si: integer;
begin
  si := Stream.Size - Stream.Position;
  si := synsock.HToNL(si);
  SendInteger(si);
  SendStreamRaw(Stream);
end;

procedure TBlockSocket.SendStream(const Stream: TStream);
var
  si: integer;
begin
  si := Stream.Size - Stream.Position;
  SendInteger(si);
  SendStreamRaw(Stream);
end;

function TBlockSocket.RecvBuffer(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := 0;
  if TestStopFlag then
    Exit;
  LimitBandwidth(Length, FMaxRecvBandwidth, FNextRecv);
//  Result := synsock.Recv(FSocket, Buffer^, Length, MSG_NOSIGNAL);
  Result := synsock.Recv(FSocket, Buffer, Length, MSG_NOSIGNAL);
  if Result = 0 then
    FLastError := WSAECONNRESET
  else
    SockCheck(Result);
  ExceptCheck;
  Inc(FRecvCounter, Result);
  DoStatus(HR_ReadCount, IntToStr(Result));
  DoReadFilter(Buffer, Result);
end;

function TBlockSocket.RecvBufferEx(Buffer: TMemory; Len: Integer;
  Timeout: Integer): Integer;
var
  s: AnsiString;
  rl, l: integer;
  ti: ULong;
{$IFDEF CIL}
  n: integer;
  b: TMemory;
{$ENDIF}
begin
  FLastError := 0;
  rl := 0;
  repeat
    ti := GetTick;
    s := RecvPacket(Timeout);
    l := Length(s);
    if (rl + l) > Len then
      l := Len - rl;
    {$IFDEF CIL}
    b := BytesOf(s);
    for n := 0 to l do
      Buffer[rl + n] := b[n];
    {$ELSE}
    Move(Pointer(s)^, IncPoint(Buffer, rl)^, l);
    {$ENDIF}
    rl := rl + l;
    if FLastError <> 0 then
      Break;
    if rl >= Len then
      Break;
    if not FInterPacketTimeout then
    begin
      Timeout := Timeout - integer(TickDelta(ti, GetTick));
      if Timeout <= 0 then
      begin
        FLastError := WSAETIMEDOUT;
        Break;
      end;
    end;
  until False;
  delete(s, 1, l);
  FBuffer := s;
  Result := rl;
end;

function TBlockSocket.RecvBufferStr(Length: Integer; Timeout: Integer): AnsiString;
var
  x: integer;
{$IFDEF CIL}
  buf: Tmemory;
{$ENDIF}
begin
  Result := '';
  if Length > 0 then
  begin
    {$IFDEF CIL}
    Setlength(Buf, Length);
    x := RecvBufferEx(buf, Length , Timeout);
    if FLastError = 0 then
    begin
      SetLength(Buf, x);
      Result := StringOf(buf);
    end
    else
      Result := '';
    {$ELSE}
    Setlength(Result, Length);
    x := RecvBufferEx(PChar(Result), Length , Timeout);
    if FLastError = 0 then
      SetLength(Result, x)
    else
      Result := '';
    {$ENDIF}
  end;
end;

function TBlockSocket.RecvPacket(Timeout: Integer): AnsiString;
var
  x: integer;
{$IFDEF CIL}
  buf: TMemory;
{$ENDIF}
begin
  Result := '';
  FLastError := 0;
  if FBuffer <> '' then
  begin
    Result := FBuffer;
    FBuffer := '';
  end
  else
  begin
    //not drain CPU on large downloads...
    Sleep(0);
    x := WaitingData;
    if x > 0 then
    begin
      {$IFDEF CIL}
      SetLength(Buf, x);
      x := RecvBuffer(Buf, x);
      if x >= 0 then
      begin
        SetLength(Buf, x);
        Result := StringOf(Buf);
      end;
      {$ELSE}
      SetLength(Result, x);
      x := RecvBuffer(Pointer(Result), x);
      if x >= 0 then
        SetLength(Result, x);
      {$ENDIF}
    end
    else
    begin
      if CanRead(Timeout) then
      begin
        x := WaitingData;
        if x = 0 then
          FLastError := WSAECONNRESET;
        if x > 0 then
        begin
          {$IFDEF CIL}
          SetLength(Buf, x);
          x := RecvBuffer(Buf, x);
          if x >= 0 then
          begin
            SetLength(Buf, x);
            result := StringOf(Buf);
          end;
          {$ELSE}
          SetLength(Result, x);
          x := RecvBuffer(Pointer(Result), x);
          if x >= 0 then
            SetLength(Result, x);
          {$ENDIF}
        end;
      end
      else
        FLastError := WSAETIMEDOUT;
    end;
  end;
  ExceptCheck;
end;


function TBlockSocket.RecvByte(Timeout: Integer): Byte;
begin
  Result := 0;
  FLastError := 0;
  if FBuffer = '' then
    FBuffer := RecvPacket(Timeout);
  if (FLastError = 0) and (FBuffer <> '') then
  begin
    Result := Ord(FBuffer[1]);
    Delete(FBuffer, 1, 1);
  end;
  ExceptCheck;
end;

function TBlockSocket.RecvInteger(Timeout: Integer): Integer;
var
  s: AnsiString;
begin
  Result := 0;
  s := RecvBufferStr(4, Timeout);
  if FLastError = 0 then
    Result := (ord(s[1]) + ord(s[2]) * 256) + (ord(s[3]) + ord(s[4]) * 256) * 65536;
end;

function TBlockSocket.RecvTerminated(Timeout: Integer; const Terminator: AnsiString): AnsiString;
var
  x: Integer;
  s: AnsiString;
  l: Integer;
  CorCRLF: Boolean;
  t: AnsiString;
  tl: integer;
  ti: ULong;
begin
  FLastError := 0;
  Result := '';
  l := Length(Terminator);
  if l = 0 then
    Exit;
  tl := l;
  CorCRLF := FConvertLineEnd and (Terminator = CRLF);
  s := '';
  x := 0;
  repeat
    //get rest of FBuffer or incomming new data...
    ti := GetTick;
    s := s + RecvPacket(Timeout);
    if FLastError <> 0 then
      Break;
    x := 0;
    if Length(s) > 0 then
      if CorCRLF then
      begin
        if FLastCR and (s[1] = LF) then
          Delete(s, 1, 1);
        if FLastLF and (s[1] = CR) then
          Delete(s, 1, 1);
        FLastCR := False;
        FLastLF := False;
        t := '';
        x := PosCRLF(s, t);
        tl := Length(t);
        if t = CR then
          FLastCR := True;
        if t = LF then
          FLastLF := True;
      end
      else
      begin
        x := pos(Terminator, s);
        tl := l;
      end;
    if (FMaxLineLength <> 0) and (Length(s) > FMaxLineLength) then
    begin
      FLastError := WSAENOBUFS;
      Break;
    end;
    if x > 0 then
      Break;
    if not FInterPacketTimeout then
    begin
      Timeout := Timeout - integer(TickDelta(ti, GetTick));
      if Timeout <= 0 then
      begin
        FLastError := WSAETIMEDOUT;
        Break;
      end;
    end;
  until False;
  if x > 0 then
  begin
    Result := Copy(s, 1, x - 1);
    Delete(s, 1, x + tl - 1);
  end;
  FBuffer := s;
  ExceptCheck;
end;

function TBlockSocket.RecvString(Timeout: Integer): AnsiString;
var
  s: AnsiString;
begin
  Result := '';
  s := RecvTerminated(Timeout, CRLF);
  if FLastError = 0 then
    Result := s;
end;

function TBlockSocket.RecvBlock(Timeout: Integer): AnsiString;
var
  x: integer;
begin
  Result := '';
  x := RecvInteger(Timeout);
  if FLastError = 0 then
    Result := RecvBufferStr(x, Timeout);
end;

procedure TBlockSocket.RecvStreamRaw(const Stream: TStream; Timeout: Integer);
var
  s: AnsiString;
begin
  repeat
    s := RecvPacket(Timeout);
    if FLastError = 0 then
      WriteStrToStream(Stream, s);
  until FLastError <> 0;
end;

procedure TBlockSocket.RecvStreamSize(const Stream: TStream; Timeout: Integer; Size: Integer);
var
  s: AnsiString;
  n: integer;
{$IFDEF CIL}
  buf: TMemory;
{$ENDIF}
begin
  for n := 1 to (Size div FSendMaxChunk) do
  begin
    {$IFDEF CIL}
    SetLength(buf, FSendMaxChunk);
    RecvBufferEx(buf, FSendMaxChunk, Timeout);
    if FLastError <> 0 then
      Exit;
    Stream.Write(buf, FSendMaxChunk);
    {$ELSE}
    s := RecvBufferStr(FSendMaxChunk, Timeout);
    if FLastError <> 0 then
      Exit;
    Stream.Write(Pchar(s)^, FSendMaxChunk);
    {$ENDIF}
  end;
  n := Size mod FSendMaxChunk;
  if n > 0 then
  begin
    {$IFDEF CIL}
    SetLength(buf, n);
    RecvBufferEx(buf, n, Timeout);
    if FLastError <> 0 then
      Exit;
    Stream.Write(buf, n);
    {$ELSE}
    s := RecvBufferStr(n, Timeout);
    if FLastError <> 0 then
      Exit;
    Stream.Write(Pchar(s)^, n);
    {$ENDIF}
  end;
end;

procedure TBlockSocket.RecvStreamIndy(const Stream: TStream; Timeout: Integer);
var
  x: integer;
begin
  x := RecvInteger(Timeout);
  x := synsock.NToHL(x);
  if FLastError = 0 then
    RecvStreamSize(Stream, Timeout, x);
end;

procedure TBlockSocket.RecvStream(const Stream: TStream; Timeout: Integer);
var
  x: integer;
begin
  x := RecvInteger(Timeout);
  if FLastError = 0 then
    RecvStreamSize(Stream, Timeout, x);
end;

function TBlockSocket.PeekBuffer(Buffer: TMemory; Length: Integer): Integer;
begin
 {$IFNDEF CIL}
//  Result := synsock.Recv(FSocket, Buffer^, Length, MSG_PEEK + MSG_NOSIGNAL);
  Result := synsock.Recv(FSocket, Buffer, Length, MSG_PEEK + MSG_NOSIGNAL);
  SockCheck(Result);
  ExceptCheck;
  {$ENDIF}
end;

function TBlockSocket.PeekByte(Timeout: Integer): Byte;
var
  s: string;
begin
 {$IFNDEF CIL}
  Result := 0;
  if CanRead(Timeout) then
  begin
    SetLength(s, 1);
    PeekBuffer(Pointer(s), 1);
    if s <> '' then
      Result := Ord(s[1]);
  end
  else
    FLastError := WSAETIMEDOUT;
  ExceptCheck;
  {$ENDIF}
end;

function TBlockSocket.SockCheck(SockResult: Integer): Integer;
begin
  FLastErrorDesc := '';
  if SockResult = integer(SOCKET_ERROR) then
  begin
    Result := synsock.WSAGetLastError;
    FLastErrorDesc := GetErrorDesc(Result);
  end
  else
    Result := 0;
  FLastError := Result;
end;

procedure TBlockSocket.ExceptCheck;
var
  e: ESynapseError;
begin
  FLastErrorDesc := GetErrorDesc(FLastError);
  if (LastError <> 0) and (LastError <> WSAEINPROGRESS)
    and (LastError <> WSAEWOULDBLOCK) then
  begin
    DoStatus(HR_Error, IntToStr(FLastError) + ',' + FLastErrorDesc);
    if FRaiseExcept then
    begin
      e := ESynapseError.Create(Format('Synapse TCP/IP Socket error %d: %s',
        [FLastError, FLastErrorDesc]));
//      e := ESynapseError.CreateFmt('Synapse TCP/IP Socket error %d: %s',
//        [FLastError, FLastErrorDesc]);
      e.ErrorCode := FLastError;
      e.ErrorMessage := FLastErrorDesc;
      raise e;
    end;
  end;
end;

function TBlockSocket.WaitingData: Integer;
var
  x: Integer;
begin
  Result := 0;
  if synsock.IoctlSocket(FSocket, FIONREAD, u_long(x)) = 0 then
    Result := x;
end;

function TBlockSocket.WaitingDataEx: Integer;
begin
  if FBuffer <> '' then
    Result := Length(FBuffer)
  else
    Result := WaitingData;
end;

procedure TBlockSocket.Purge;
begin
  try
    repeat
      RecvPacket(0);
    until FLastError <> 0;
  except
    on exception do;
  end;
  FLastError := 0;
end;

procedure TBlockSocket.SetLinger(Enable: Boolean; Linger: Integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_Linger;
  d.Enabled := Enable;
  d.Value := Linger;
  DelayedOption(d);
end;

function TBlockSocket.LocalName: string;
begin
  Result := synsock.GetHostName;
  if Result = '' then
    Result := '127.0.0.1';
end;

{$IFDEF CIL}
procedure TBlockSocket.ResolveNameToIP(Name: string; const IPList: TStrings);
var
  IPs :array of IPAddress;
  n: integer;
begin
  IPList.Clear;
  IPs := Dns.Resolve(Name).AddressList;
  for n := low(IPs) to high(IPs) do
  begin
    if not(((FFamily = SF_IP6) and (IPs[n].AddressFamily = AF_INET))
      or ((FFamily = SF_IP4) and (IPs[n].AddressFamily = AF_INET6))) then
    begin
      IPList.Add(IPs[n].toString);
    end;
  end;
end;

{$ELSE}
procedure TBlockSocket.ResolveNameToIP(Name: string; const IPList: TStrings);
type
  TaPInAddr = array[0..250] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  AddrNext: PAddrInfo;
  r: integer;
  host, serv: string;
  hostlen, servlen: integer;
  RemoteHost: PHostEnt;
  IP: u_long;
  PAdrPtr: PaPInAddr;
  i: Integer;
  s: string;
  InAddr: TInAddr;
begin
  IPList.Clear;
  if not IsNewApi then
  begin
    IP := synsock.inet_addr(PChar(Name));
    if IP = u_long(INADDR_NONE) then
    begin
      SynSockCS.Enter;
      try
        RemoteHost := synsock.GetHostByName(PChar(Name));
        if RemoteHost <> nil then
        begin
          PAdrPtr := PAPInAddr(RemoteHost^.h_addr_list);
          i := 0;
          while PAdrPtr^[i] <> nil do
          begin
            InAddr := PAdrPtr^[i]^;
            with InAddr.S_un_b do
              s := Format('%d.%d.%d.%d',
                [Ord(s_b1), Ord(s_b2), Ord(s_b3), Ord(s_b4)]);
            IPList.Add(s);
            Inc(i);
          end;
        end;
      finally
        SynSockCS.Leave;
      end;
    end
    else
      IPList.Add(Name);
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_UNSPEC;
      Hints.ai_socktype := GetSocketType;
      Hints.ai_protocol := GetSocketprotocol;
      Hints.ai_flags := 0;
      r := synsock.GetAddrInfo(PChar(Name), nil, @Hints, Addr);
      if r = 0 then
      begin
        AddrNext := Addr;
        while not(AddrNext = nil) do
        begin
          if not(((FFamily = SF_IP6) and (AddrNext^.ai_family = AF_INET))
            or ((FFamily = SF_IP4) and (AddrNext^.ai_family = AF_INET6))) then
          begin
            hostlen := NI_MAXHOST;
            servlen := NI_MAXSERV;
            setlength(host, hostlen);
            setlength(serv, servlen);
            r := getnameinfo(AddrNext^.ai_addr, AddrNext^.ai_addrlen,
              PChar(host), hostlen, PChar(serv), servlen,
              NI_NUMERICHOST + NI_NUMERICSERV);
            if r = 0 then
            begin
              host := PChar(host);
              IPList.Add(host);
            end;
          end;
          AddrNext := AddrNext^.ai_next;
        end;
      end;
    finally
      if Assigned(Addr) then
        synsock.FreeAddrInfo(Addr);
    end;
  end;
  if IPList.Count = 0 then
    IPList.Add(cAnyHost);
end;
{$ENDIF}

function TBlockSocket.ResolveName(Name: string): string;
var
  l: TStringList;
begin
  l := TStringList.Create;
  try
    ResolveNameToIP(Name, l);
    Result := l[0];
  finally
    l.Free;
  end;
end;

function TBlockSocket.ResolvePort(Port: string): Word;
{$IFDEF CIL}
begin
  Result := SynSock.GetPortService(Port);
end;
{$ELSE}
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  r: integer;
begin
  Result := 0;
  if not IsNewApi then
  begin
    SynSockCS.Enter;
    try
      ProtoEnt := synsock.GetProtoByNumber(GetSocketProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
        ServEnt := synsock.GetServByName(PChar(Port), ProtoEnt^.p_name);
      if ServEnt = nil then
        Result := StrToIntDef(Port, 0)
      else
        Result := synsock.htons(ServEnt^.s_port);
    finally
      SynSockCS.Leave;
    end;
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_UNSPEC;
      Hints.ai_socktype := GetSocketType;
      Hints.ai_protocol := GetSocketprotocol;
      Hints.ai_flags := AI_PASSIVE;
      r := synsock.GetAddrInfo(nil, PChar(Port), @Hints, Addr);
      if r = 0 then
      begin
        if Addr^.ai_family = AF_INET then
          Result := synsock.htons(Addr^.ai_addr^.sin_port);
        if Addr^.ai_family = AF_INET6 then
          Result := synsock.htons(PSockAddrIn6(Addr^.ai_addr)^.sin6_port);
      end;
    finally
      if Assigned(Addr) then
        synsock.FreeAddrInfo(Addr);
    end;
  end;
end;
{$ENDIF}

{$IFDEF CIL}
function TBlockSocket.ResolveIPToName(IP: string): string;
begin
  Result := Dns.GetHostByAddress(IP).HostName;
end;
{$ELSE}
function TBlockSocket.ResolveIPToName(IP: string): string;
var
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  r: integer;
  host, serv: string;
  hostlen, servlen: integer;
  RemoteHost: PHostEnt;
  IPn: u_long;
begin
  Result := IP;
  if not IsNewApi then
  begin
    if not IsIP(IP) then
      IP := ResolveName(IP);
    IPn := synsock.inet_addr(PChar(IP));
    if IPn <> u_long(INADDR_NONE) then
    begin
      SynSockCS.Enter;
      try
        RemoteHost := GetHostByAddr(@IPn, SizeOf(IPn), AF_INET);
        if RemoteHost <> nil then
          Result := RemoteHost^.h_name;
      finally
        SynSockCS.Leave;
      end;
    end;
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_UNSPEC;
      Hints.ai_socktype := GetSocketType;
      Hints.ai_protocol := GetSocketprotocol;
      Hints.ai_flags := 0;
      r := synsock.GetAddrInfo(PChar(IP), nil, @Hints, Addr);
      if r = 0 then
      begin
        hostlen := NI_MAXHOST;
        servlen := NI_MAXSERV;
        setlength(host, hostlen);
        setlength(serv, servlen);
        r := getnameinfo(Addr^.ai_addr, Addr^.ai_addrlen,
          PChar(host), hostlen, PChar(serv), servlen,
          NI_NUMERICSERV);
        if r = 0 then
          Result := PChar(host);
      end;
    finally
      if Assigned(Addr) then
        synsock.FreeAddrInfo(Addr);
    end;
  end;
end;
{$ENDIF}

procedure TBlockSocket.SetRemoteSin(IP, Port: string);
begin
  SetSin(FRemoteSin, IP, Port);
end;

function TBlockSocket.GetLocalSinIP: string;
begin
  Result := GetSinIP(FLocalSin);
end;

function TBlockSocket.GetRemoteSinIP: string;
begin
  Result := GetSinIP(FRemoteSin);
end;

function TBlockSocket.GetLocalSinPort: Integer;
begin
  Result := GetSinPort(FLocalSin);
end;

function TBlockSocket.GetRemoteSinPort: Integer;
begin
  Result := GetSinPort(FRemoteSin);
end;

function TBlockSocket.CanRead(Timeout: Integer): Boolean;
{$IFDEF CIL}
begin
  Result := FSocket.Poll(Timeout * 1000, SelectMode.SelectRead);
{$ELSE}
var
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
  FDSet: TFDSet;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FDSet := FFdSet;
  x := synsock.Select(FSocket + 1, @FDSet, nil, nil, TimeVal);
  SockCheck(x);
  if FLastError <> 0 then
    x := 0;
  Result := x > 0;
{$ENDIF}
  ExceptCheck;
  if Result then
    DoStatus(HR_CanRead, '');
end;

function TBlockSocket.CanWrite(Timeout: Integer): Boolean;
{$IFDEF CIL}
begin
  Result := FSocket.Poll(Timeout * 1000, SelectMode.SelectWrite);
{$ELSE}
var
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
  FDSet: TFDSet;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FDSet := FFdSet;
  x := synsock.Select(FSocket + 1, nil, @FDSet, nil, TimeVal);
  SockCheck(x);
  if FLastError <> 0 then
    x := 0;
  Result := x > 0;
{$ENDIF}
  ExceptCheck;
  if Result then
    DoStatus(HR_CanWrite, '');
end;

function TBlockSocket.CanReadEx(Timeout: Integer): Boolean;
begin
  if FBuffer <> '' then
    Result := True
  else
    Result := CanRead(Timeout);
end;

function TBlockSocket.SendBufferTo(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := 0;
  if TestStopFlag then
    Exit;
  LimitBandwidth(Length, FMaxSendBandwidth, FNextsend);
  Result := synsock.SendTo(FSocket, Buffer, Length, 0, FRemoteSin);
  SockCheck(Result);
  ExceptCheck;
  Inc(FSendCounter, Result);
  DoStatus(HR_WriteCount, IntToStr(Result));
end;

function TBlockSocket.RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := 0;
  if TestStopFlag then
    Exit;
  LimitBandwidth(Length, FMaxRecvBandwidth, FNextRecv);
  Result := synsock.RecvFrom(FSocket, Buffer, Length, 0, FRemoteSin);
  SockCheck(Result);
  ExceptCheck;
  Inc(FRecvCounter, Result);
  DoStatus(HR_ReadCount, IntToStr(Result));
end;

function TBlockSocket.GetSizeRecvBuffer: Integer;
var
  l: Integer;
{$IFDEF CIL}
  buf: TMemory;
{$ENDIF}
begin
{$IFDEF CIL}
  setlength(buf, 4);
  SockCheck(synsock.GetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVBUF), buf, l));
  Result := System.BitConverter.ToInt32(buf,0);
{$ELSE}
  l := SizeOf(Result);
  SockCheck(synsock.GetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, @Result, l));
  if FLastError <> 0 then
    Result := 1024;
  ExceptCheck;
{$ENDIF}
end;

procedure TBlockSocket.SetSizeRecvBuffer(Size: Integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_RecvBuff;
  d.Value := Size;
  DelayedOption(d);
end;

function TBlockSocket.GetSizeSendBuffer: Integer;
var
  l: Integer;
{$IFDEF CIL}
  buf: TMemory;
{$ENDIF}
begin
{$IFDEF CIL}
  setlength(buf, 4);
  SockCheck(synsock.GetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_SNDBUF), buf, l));
  Result := System.BitConverter.ToInt32(buf,0);
{$ELSE}
  l := SizeOf(Result);
  SockCheck(synsock.GetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, @Result, l));
  if FLastError <> 0 then
    Result := 1024;
  ExceptCheck;
{$ENDIF}
end;

procedure TBlockSocket.SetSizeSendBuffer(Size: Integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_SendBuff;
  d.Value := Size;
  DelayedOption(d);
end;

procedure TBlockSocket.SetNonBlockMode(Value: Boolean);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_nonblock;
  d.Enabled := Value;
  DelayedOption(d);
end;

procedure TBlockSocket.SetTimeout(Timeout: Integer);
begin
  SetSendTimeout(Timeout);
  SetRecvTimeout(Timeout);
end;

procedure TBlockSocket.SetSendTimeout(Timeout: Integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_sendtimeout;
  d.Value := Timeout;
  DelayedOption(d);
end;

procedure TBlockSocket.SetRecvTimeout(Timeout: Integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_recvtimeout;
  d.Value := Timeout;
  DelayedOption(d);
end;

{$IFNDEF CIL}
function TBlockSocket.GroupCanRead(const SocketList: TList; Timeout: Integer;
  const CanReadList: TList): boolean;
var
  FDSet: TFDSet;
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x, n: Integer;
  Max: Integer;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FD_ZERO(FDSet);
  Max := 0;
  for n := 0 to SocketList.Count - 1 do
    if TObject(SocketList.Items[n]) is TBlockSocket then
    begin
      if TBlockSocket(SocketList.Items[n]).Socket > Max then
        Max := TBlockSocket(SocketList.Items[n]).Socket;
      FD_SET(TBlockSocket(SocketList.Items[n]).Socket, FDSet);
    end;
  x := synsock.Select(Max + 1, @FDSet, nil, nil, TimeVal);
  SockCheck(x);
  ExceptCheck;
  if FLastError <> 0 then
    x := 0;
  Result := x > 0;
  CanReadList.Clear;
  if Result then
    for n := 0 to SocketList.Count - 1 do
      if TObject(SocketList.Items[n]) is TBlockSocket then
        if FD_ISSET(TBlockSocket(SocketList.Items[n]).Socket, FDSet) then
          CanReadList.Add(TBlockSocket(SocketList.Items[n]));
end;
{$ENDIF}

procedure TBlockSocket.EnableReuse(Value: Boolean);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_reuse;
  d.Enabled := Value;
  DelayedOption(d);
end;

procedure TBlockSocket.SetTTL(TTL: integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_TTL;
  d.Value := TTL;
  DelayedOption(d);
end;

function TBlockSocket.GetTTL:integer;
var
  l: Integer;
begin
{$IFNDEF CIL}
  l := SizeOf(Result);
  if FIP6Used then
    synsock.GetSockOpt(FSocket, IPPROTO_IPV6, IPV6_UNICAST_HOPS, @Result, l)
  else
    synsock.GetSockOpt(FSocket, IPPROTO_IP, IP_TTL, @Result, l);
{$ENDIF}
end;

procedure TBlockSocket.SetFamily(Value: TSocketFamily);
begin
  FFamily := Value;
  FFamilySave := Value;
end;

procedure TBlockSocket.SetSocket(Value: TSocket);
begin
  FRecvCounter := 0;
  FSendCounter := 0;
  FSocket := Value;
{$IFNDEF CIL}
  FD_ZERO(FFDSet);
  FD_SET(FSocket, FFDSet);
{$ENDIF}
  GetSins;
  FIP6Used := FRemoteSin.AddressFamily = AF_INET6;
end;

{$IFDEF CIL}
function TBlockSocket.StrToIP6(const value: string): TSockAddrIn6;
var
  buf: TMemory;
  IP: IPAddress;
begin
  IP := IPAddress.Parse(Value);
  buf := IP.GetAddressBytes;
  result.sin6_addr.S_un_b.s_b1 := char(buf[0]);
  result.sin6_addr.S_un_b.s_b2 := char(buf[1]);
  result.sin6_addr.S_un_b.s_b3 := char(buf[2]);
  result.sin6_addr.S_un_b.s_b4 := char(buf[3]);
  result.sin6_addr.S_un_b.s_b5 := char(buf[4]);
  result.sin6_addr.S_un_b.s_b6 := char(buf[5]);
  result.sin6_addr.S_un_b.s_b7 := char(buf[6]);
  result.sin6_addr.S_un_b.s_b8 := char(buf[7]);
  result.sin6_addr.S_un_b.s_b9 := char(buf[8]);
  result.sin6_addr.S_un_b.s_b10 := char(buf[9]);
  result.sin6_addr.S_un_b.s_b11 := char(buf[10]);
  result.sin6_addr.S_un_b.s_b12 := char(buf[11]);
  result.sin6_addr.S_un_b.s_b13 := char(buf[12]);
  result.sin6_addr.S_un_b.s_b14 := char(buf[13]);
  result.sin6_addr.S_un_b.s_b15 := char(buf[14]);
  result.sin6_addr.S_un_b.s_b16 := char(buf[15]);
  result.sin6_family := Word(AF_INET6);
end;
{$ELSE}
function TBlockSocket.StrToIP6(const value: string): TSockAddrIn6;
var
  addr: PAddrInfo;
  hints: TAddrInfo;
  r: integer;
begin
  FillChar(Result, Sizeof(Result), 0);
  if SockEnhancedApi or SockWship6Api then
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_INET6;
      Hints.ai_flags := AI_NUMERICHOST;
      r := synsock.GetAddrInfo(PChar(value), nil, @Hints, Addr);
      if r = 0 then
        if (Addr^.ai_family = AF_INET6) then
            Move(Addr^.ai_addr^, Result, SizeOf(Result));
    finally
      if Assigned(Addr) then
        synsock.FreeAddrInfo(Addr);
    end;
  end;
end;
{$ENDIF}

{$IFDEF CIL}
function TBlockSocket.IP6ToStr(const value: TSockAddrIn6): string;
var
  buf: TMemory;
  IP: IPAddress;
begin
  setlength(buf, 16);
  buf[0] := byte(value.sin6_addr.S_un_b.s_b1);
  buf[1] := byte(value.sin6_addr.S_un_b.s_b2);
  buf[2] := byte(value.sin6_addr.S_un_b.s_b3);
  buf[3] := byte(value.sin6_addr.S_un_b.s_b4);
  buf[4] := byte(value.sin6_addr.S_un_b.s_b5);
  buf[5] := byte(value.sin6_addr.S_un_b.s_b6);
  buf[6] := byte(value.sin6_addr.S_un_b.s_b7);
  buf[7] := byte(value.sin6_addr.S_un_b.s_b8);
  buf[8] := byte(value.sin6_addr.S_un_b.s_b9);
  buf[9] := byte(value.sin6_addr.S_un_b.s_b10);
  buf[10] := byte(value.sin6_addr.S_un_b.s_b11);
  buf[11] := byte(value.sin6_addr.S_un_b.s_b12);
  buf[12] := byte(value.sin6_addr.S_un_b.s_b13);
  buf[13] := byte(value.sin6_addr.S_un_b.s_b14);
  buf[14] := byte(value.sin6_addr.S_un_b.s_b15);
  buf[15] := byte(value.sin6_addr.S_un_b.s_b16);
  IP := IPAddress.Create(buf);
  Result := IP.ToString;
end;
{$ELSE}
function TBlockSocket.IP6ToStr(const value: TSockAddrIn6): string;
var
  host, serv: string;
  hostlen, servlen: integer;
  r: integer;
begin
  Result := '';
  if SockEnhancedApi or SockWship6Api then
  begin
    hostlen := NI_MAXHOST;
    servlen := NI_MAXSERV;
    setlength(host, hostlen);
    setlength(serv, servlen);
    r := getnameinfo(@Value, SizeOf(value), PChar(host), hostlen,
      PChar(serv), servlen, NI_NUMERICHOST + NI_NUMERICSERV);
    if r = 0 then
      Result := PChar(host);
  end;
end;
{$ENDIF}

function TBlockSocket.GetWsaData: TWSAData;
begin
  Result := WsaDataOnce;
end;

function TBlockSocket.GetSocketType: integer;
begin
  Result := 0;
end;

function TBlockSocket.GetSocketProtocol: integer;
begin
  Result := integer(IPPROTO_IP);
end;

procedure TBlockSocket.DoStatus(Reason: THookSocketReason; const Value: string);
begin
  if assigned(OnStatus) then
    OnStatus(Self, Reason, Value);
end;

procedure TBlockSocket.DoReadFilter(Buffer: TMemory; var Len: Integer);
var
  s: string;
begin
  if assigned(OnReadFilter) then
    if Len > 0 then
      begin
        {$IFDEF CIL}
        s := StringOf(Buffer);
        {$ELSE}
        SetLength(s, Len);
        Move(Buffer^, Pointer(s)^, Len);
        {$ENDIF}
        OnReadFilter(Self, s);
        if Length(s) > Len then
          SetLength(s, Len);
        Len := Length(s);
        {$IFDEF CIL}
        Buffer := BytesOf(s);
        {$ELSE}
        Move(Pointer(s)^, Buffer^, Len);
        {$ENDIF}
      end;
end;

procedure TBlockSocket.DoWriteFilter(Buffer: TMemory; var Len: Integer);
var
  s: string;
begin
  if assigned(OnWriteFilter) then
    if Len > 0 then
      begin
        {$IFDEF CIL}
        s := StringOf(Buffer);
        {$ELSE}
        SetLength(s, Len);
        Move(Buffer^, Pointer(s)^, Len);
        {$ENDIF}
        OnWriteFilter(Self, s);
        if Length(s) > Len then
          SetLength(s, Len);
        Len := Length(s);
        {$IFDEF CIL}
        Buffer := BytesOf(s);
        {$ELSE}
        Move(Pointer(s)^, Buffer^, Len);
        {$ENDIF}
      end;
end;

procedure TBlockSocket.DoCreateSocket;
begin
  if assigned(OnCreateSocket) then
    OnCreateSocket(Self);
end;

class function TBlockSocket.GetErrorDesc(ErrorCode: Integer): string;
begin
{$IFDEF CIL}
  if ErrorCode = 0 then
    Result := ''
  else
  begin
    Result := WSAGetLastErrorDesc;
    if Result = '' then
      Result := 'Other Winsock error (' + IntToStr(ErrorCode) + ')';
  end;
{$ELSE}
  case ErrorCode of
    0:
      Result := '';
    WSAEINTR: {10004}
      Result := 'Interrupted system call';
    WSAEBADF: {10009}
      Result := 'Bad file number';
    WSAEACCES: {10013}
      Result := 'Permission denied';
    WSAEFAULT: {10014}
      Result := 'Bad address';
    WSAEINVAL: {10022}
      Result := 'Invalid argument';
    WSAEMFILE: {10024}
      Result := 'Too many open files';
    WSAEWOULDBLOCK: {10035}
      Result := 'Operation would block';
    WSAEINPROGRESS: {10036}
      Result := 'Operation now in progress';
    WSAEALREADY: {10037}
      Result := 'Operation already in progress';
    WSAENOTSOCK: {10038}
      Result := 'Socket operation on nonsocket';
    WSAEDESTADDRREQ: {10039}
      Result := 'Destination address required';
    WSAEMSGSIZE: {10040}
      Result := 'Message too long';
    WSAEPROTOTYPE: {10041}
      Result := 'Protocol wrong type for Socket';
    WSAENOPROTOOPT: {10042}
      Result := 'Protocol not available';
    WSAEPROTONOSUPPORT: {10043}
      Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT: {10044}
      Result := 'Socket not supported';
    WSAEOPNOTSUPP: {10045}
      Result := 'Operation not supported on Socket';
    WSAEPFNOSUPPORT: {10046}
      Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT: {10047}
      Result := 'Address family not supported';
    WSAEADDRINUSE: {10048}
      Result := 'Address already in use';
    WSAEADDRNOTAVAIL: {10049}
      Result := 'Can''t assign requested address';
    WSAENETDOWN: {10050}
      Result := 'Network is down';
    WSAENETUNREACH: {10051}
      Result := 'Network is unreachable';
    WSAENETRESET: {10052}
      Result := 'Network dropped connection on reset';
    WSAECONNABORTED: {10053}
      Result := 'Software caused connection abort';
    WSAECONNRESET: {10054}
      Result := 'Connection reset by peer';
    WSAENOBUFS: {10055}
      Result := 'No Buffer space available';
    WSAEISCONN: {10056}
      Result := 'Socket is already connected';
    WSAENOTCONN: {10057}
      Result := 'Socket is not connected';
    WSAESHUTDOWN: {10058}
      Result := 'Can''t send after Socket shutdown';
    WSAETOOMANYREFS: {10059}
      Result := 'Too many references:can''t splice';
    WSAETIMEDOUT: {10060}
      Result := 'Connection timed out';
    WSAECONNREFUSED: {10061}
      Result := 'Connection refused';
    WSAELOOP: {10062}
      Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG: {10063}
      Result := 'File name is too long';
    WSAEHOSTDOWN: {10064}
      Result := 'Host is down';
    WSAEHOSTUNREACH: {10065}
      Result := 'No route to host';
    WSAENOTEMPTY: {10066}
      Result := 'Directory is not empty';
    WSAEPROCLIM: {10067}
      Result := 'Too many processes';
    WSAEUSERS: {10068}
      Result := 'Too many users';
    WSAEDQUOT: {10069}
      Result := 'Disk quota exceeded';
    WSAESTALE: {10070}
      Result := 'Stale NFS file handle';
    WSAEREMOTE: {10071}
      Result := 'Too many levels of remote in path';
    WSASYSNOTREADY: {10091}
      Result := 'Network subsystem is unusable';
    WSAVERNOTSUPPORTED: {10092}
      Result := 'Winsock DLL cannot support this application';
    WSANOTINITIALISED: {10093}
      Result := 'Winsock not initialized';
    WSAEDISCON: {10101}
      Result := 'Disconnect';
    WSAHOST_NOT_FOUND: {11001}
      Result := 'Host not found';
    WSATRY_AGAIN: {11002}
      Result := 'Non authoritative - host not found';
    WSANO_RECOVERY: {11003}
      Result := 'Non recoverable error';
    WSANO_DATA: {11004}
      Result := 'Valid name, no data record of requested type'
  else
    Result := 'Other Winsock error (' + IntToStr(ErrorCode) + ')';
  end;
{$ENDIF}
end;

{======================================================================}

constructor TSocksBlockSocket.Create;
begin
  inherited Create;
  FSocksIP:= '';
  FSocksPort:= '1080';
  FSocksTimeout:= 60000;
  FSocksUsername:= '';
  FSocksPassword:= '';
  FUsingSocks := False;
  FSocksResolver := True;
  FSocksLastError := 0;
  FSocksResponseIP := '';
  FSocksResponsePort := '';
  FSocksLocalIP := '';
  FSocksLocalPort := '';
  FSocksRemoteIP := '';
  FSocksRemotePort := '';
  FBypassFlag := False;
  FSocksType := ST_Socks5;
end;

function TSocksBlockSocket.SocksOpen: boolean;
var
  Buf: string;
  n: integer;
begin
  Result := False;
  FUsingSocks := False;
  if FSocksType <> ST_Socks5 then
  begin
    FUsingSocks := True;
    Result := True;
  end
  else
  begin
    FBypassFlag := True;
    try
      if FSocksUsername = '' then
        Buf := #5 + #1 + #0
      else
        Buf := #5 + #2 + #2 +#0;
      SendString(Buf);
      Buf := RecvBufferStr(2, FSocksTimeout);
      if Length(Buf) < 2 then
        Exit;
      if Buf[1] <> #5 then
        Exit;
      n := Ord(Buf[2]);
      case n of
        0: //not need authorisation
          ;
        2:
          begin
            Buf := #1 + char(Length(FSocksUsername)) + FSocksUsername
              + char(Length(FSocksPassword)) + FSocksPassword;
            SendString(Buf);
            Buf := RecvBufferStr(2, FSocksTimeout);
            if Length(Buf) < 2 then
              Exit;
            if Buf[2] <> #0 then
              Exit;
          end;
      else
        //other authorisation is not supported!
        Exit;
      end;
      FUsingSocks := True;
      Result := True;
    finally
      FBypassFlag := False;
    end;
  end;
end;

function TSocksBlockSocket.SocksRequest(Cmd: Byte;
  const IP, Port: string): Boolean;
var
  Buf: string;
begin
  FBypassFlag := True;
  try
    if FSocksType <> ST_Socks5 then
      Buf := #4 + char(Cmd) + SocksCode(IP, Port)
    else
      Buf := #5 + char(Cmd) + #0 + SocksCode(IP, Port);
    SendString(Buf);
    Result := FLastError = 0;
  finally
    FBypassFlag := False;
  end;
end;

function TSocksBlockSocket.SocksResponse: Boolean;
var
  Buf, s: string;
  x: integer;
begin
  Result := False;
  FBypassFlag := True;
  try
    FSocksResponseIP := '';
    FSocksResponsePort := '';
    FSocksLastError := -1;
    if FSocksType <> ST_Socks5 then
    begin
      Buf := RecvBufferStr(8, FSocksTimeout);
      if FLastError <> 0 then
        Exit;
      if Buf[1] <> #0 then
        Exit;
      FSocksLastError := Ord(Buf[2]);
    end
    else
    begin
      Buf := RecvBufferStr(4, FSocksTimeout);
      if FLastError <> 0 then
        Exit;
      if Buf[1] <> #5 then
        Exit;
      case Ord(Buf[4]) of
        1:
          s := RecvBufferStr(4, FSocksTimeout);
        3:
          begin
            x := RecvByte(FSocksTimeout);
            if FLastError <> 0 then
              Exit;
            s := char(x) + RecvBufferStr(x, FSocksTimeout);
          end;
        4:
          s := RecvBufferStr(16, FSocksTimeout);
      else
        Exit;
      end;
      Buf := Buf + s + RecvBufferStr(2, FSocksTimeout);
      if FLastError <> 0 then
        Exit;
      FSocksLastError := Ord(Buf[2]);
    end;
    if ((FSocksLastError <> 0) and (FSocksLastError <> 90)) then
      Exit;
    SocksDecode(Buf);
    Result := True;
  finally
    FBypassFlag := False;
  end;
end;

function TSocksBlockSocket.SocksCode(IP, Port: string): string;
var
  s: string;
  ip6: TSockAddrIn6;
begin
  if FSocksType <> ST_Socks5 then
  begin
    Result := CodeInt(ResolvePort(Port));
    if not FSocksResolver then
      IP := ResolveName(IP);
    if IsIP(IP) then
    begin
      Result := Result + IPToID(IP);
      Result := Result + FSocksUsername + #0;
    end
    else
    begin
      Result := Result + IPToID('0.0.0.1');
      Result := Result + FSocksUsername + #0;
      Result := Result + IP + #0;
    end;
  end
  else
  begin
    if not FSocksResolver then
      IP := ResolveName(IP);
    if IsIP(IP) then
      Result := #1 + IPToID(IP)
    else
      if IsIP6(IP) then
      begin
        ip6 := StrToIP6(IP);
        setlength(s, 16);
        s[1] := ip6.sin6_addr.S_un_b.s_b1;
        s[2] := ip6.sin6_addr.S_un_b.s_b2;
        s[3] := ip6.sin6_addr.S_un_b.s_b3;
        s[4] := ip6.sin6_addr.S_un_b.s_b4;
        s[5] := ip6.sin6_addr.S_un_b.s_b5;
        s[6] := ip6.sin6_addr.S_un_b.s_b6;
        s[7] := ip6.sin6_addr.S_un_b.s_b7;
        s[8] := ip6.sin6_addr.S_un_b.s_b8;
        s[9] := ip6.sin6_addr.S_un_b.s_b9;
        s[10] := ip6.sin6_addr.S_un_b.s_b10;
        s[11] := ip6.sin6_addr.S_un_b.s_b11;
        s[12] := ip6.sin6_addr.S_un_b.s_b12;
        s[13] := ip6.sin6_addr.S_un_b.s_b13;
        s[14] := ip6.sin6_addr.S_un_b.s_b14;
        s[15] := ip6.sin6_addr.S_un_b.s_b15;
        s[16] := ip6.sin6_addr.S_un_b.s_b16;
        Result := #4 + s;
      end
      else
        Result := #3 + char(Length(IP)) + IP;
    Result := Result + CodeInt(ResolvePort(Port));
  end;
end;

function TSocksBlockSocket.SocksDecode(Value: string): integer;
var
  Atyp: Byte;
  y, n: integer;
  w: Word;
  ip6: TSockAddrIn6;
begin
  FSocksResponsePort := '0';
  Result := 0;
  if FSocksType <> ST_Socks5 then
  begin
    if Length(Value) < 8 then
      Exit;
    Result := 3;
    w := DecodeInt(Value, Result);
    FSocksResponsePort := IntToStr(w);
    FSocksResponseIP := Format('%d.%d.%d.%d',
      [Ord(Value[5]), Ord(Value[6]), Ord(Value[7]), Ord(Value[8])]);
    Result := 9;
  end
  else
  begin
    if Length(Value) < 4 then
      Exit;
    Atyp := Ord(Value[4]);
    Result := 5;
    case Atyp of
      1:
        begin
          if Length(Value) < 10 then
            Exit;
          FSocksResponseIP := Format('%d.%d.%d.%d',
              [Ord(Value[5]), Ord(Value[6]), Ord(Value[7]), Ord(Value[8])]);
          Result := 9;
        end;
      3:
        begin
          y := Ord(Value[5]);
          if Length(Value) < (5 + y + 2) then
            Exit;
          for n := 6 to 6 + y - 1 do
            FSocksResponseIP := FSocksResponseIP + Value[n];
          Result := 5 + y + 1;
        end;
      4:
        begin
          if Length(Value) < 22 then
            Exit;
          ip6.sin6_addr.S_un_b.s_b1 := Value[5];
          ip6.sin6_addr.S_un_b.s_b2 := Value[6];
          ip6.sin6_addr.S_un_b.s_b3 := Value[7];
          ip6.sin6_addr.S_un_b.s_b4 := Value[8];
          ip6.sin6_addr.S_un_b.s_b5 := Value[9];
          ip6.sin6_addr.S_un_b.s_b6 := Value[10];
          ip6.sin6_addr.S_un_b.s_b7 := Value[11];
          ip6.sin6_addr.S_un_b.s_b8 := Value[12];
          ip6.sin6_addr.S_un_b.s_b9 := Value[13];
          ip6.sin6_addr.S_un_b.s_b10 := Value[14];
          ip6.sin6_addr.S_un_b.s_b11 := Value[15];
          ip6.sin6_addr.S_un_b.s_b12 := Value[16];
          ip6.sin6_addr.S_un_b.s_b13 := Value[17];
          ip6.sin6_addr.S_un_b.s_b14 := Value[18];
          ip6.sin6_addr.S_un_b.s_b15 := Value[19];
          ip6.sin6_addr.S_un_b.s_b16 := Value[20];
          ip6.sin6_family := word(AF_INET6);
          ip6.sin6_port := 0;
          ip6.sin6_flowinfo := 0;
          ip6.sin6_scope_id := 0;
          FSocksResponseIP := IP6ToStr(ip6);
          Result := 21;
        end;
    else
      Exit;
    end;
    w := DecodeInt(Value, Result);
    FSocksResponsePort := IntToStr(w);
    Result := Result + 2;
  end;
end;

{======================================================================}

procedure TDgramBlockSocket.Connect(IP, Port: string);
begin
  SetRemoteSin(IP, Port);
  InternalCreateSocket(FRemoteSin);
  FBuffer := '';
  DoStatus(HR_Connect, IP + ':' + Port);
end;

function TDgramBlockSocket.RecvBuffer(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := RecvBufferFrom(Buffer, Length);
end;

function TDgramBlockSocket.SendBuffer(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := SendBufferTo(Buffer, Length);
end;

{======================================================================}

destructor TUDPBlockSocket.Destroy;
begin
  if Assigned(FSocksControlSock) then
    FSocksControlSock.Free;
  inherited;
end;

procedure TUDPBlockSocket.EnableBroadcast(Value: Boolean);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_Broadcast;
  d.Enabled := Value;
  DelayedOption(d);
end;

function TUDPBlockSocket.UdpAssociation: Boolean;
var
  b: Boolean;
begin
  Result := True;
  FUsingSocks := False;
  if FSocksIP <> '' then
  begin
    Result := False;
    if not Assigned(FSocksControlSock) then
      FSocksControlSock := TTCPBlockSocket.Create;
    FSocksControlSock.CloseSocket;
    FSocksControlSock.CreateSocketByName(FSocksIP);
    FSocksControlSock.Connect(FSocksIP, FSocksPort);
    if FSocksControlSock.LastError <> 0 then
      Exit;
    // if not assigned local port, assign it!
    if not FBinded then
      Bind(cAnyHost, cAnyPort);
    //open control TCP connection to SOCKS
    FSocksControlSock.FSocksUsername := FSocksUsername;
    FSocksControlSock.FSocksPassword := FSocksPassword;
    b := FSocksControlSock.SocksOpen;
    if b then
      b := FSocksControlSock.SocksRequest(3, GetLocalSinIP, IntToStr(GetLocalSinPort));
    if b then
      b := FSocksControlSock.SocksResponse;
    if not b and (FLastError = 0) then
      FLastError := WSANO_RECOVERY;
    FUsingSocks :=FSocksControlSock.UsingSocks;
    FSocksRemoteIP := FSocksControlSock.FSocksResponseIP;
    FSocksRemotePort := FSocksControlSock.FSocksResponsePort;
    Result := b and (FLastError = 0);
  end;
end;

function TUDPBlockSocket.SendBufferTo(Buffer: TMemory; Length: Integer): Integer;
var
  SIp: string;
  SPort: integer;
  Buf: string;
begin
  Result := 0;
  FUsingSocks := False;
  if (FSocksIP <> '') and (not UdpAssociation) then
    FLastError := WSANO_RECOVERY
  else
  begin
    if FUsingSocks then
    begin
{$IFNDEF CIL}
      Sip := GetRemoteSinIp;
      SPort := GetRemoteSinPort;
      SetRemoteSin(FSocksRemoteIP, FSocksRemotePort);
      SetLength(Buf,Length);
      Move(Buffer^, PChar(Buf)^, Length);
      Buf := #0 + #0 + #0 + SocksCode(Sip, IntToStr(SPort)) + Buf;
      Result := inherited SendBufferTo(PChar(Buf), System.Length(buf));
      SetRemoteSin(Sip, IntToStr(SPort));
{$ENDIF}
    end
    else
      Result := inherited SendBufferTo(Buffer, Length);
  end;
end;

function TUDPBlockSocket.RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer;
var
  Buf: string;
  x: integer;
begin
  Result := inherited RecvBufferFrom(Buffer, Length);
  if FUsingSocks then
  begin
{$IFNDEF CIL}
    SetLength(Buf, Result);
    Move(Buffer^, PChar(Buf)^, Result);
    x := SocksDecode(Buf);
    Result := Result - x + 1;
    Buf := Copy(Buf, x, Result);
    Move(PChar(Buf)^, Buffer^, Result);
    SetRemoteSin(FSocksResponseIP, FSocksResponsePort);
{$ENDIF}
  end;
end;

{$IFNDEF CIL}
procedure TUDPBlockSocket.AddMulticast(MCastIP: string);
var
  Multicast: TIP_mreq;
  Multicast6: TIPv6_mreq;
begin
  if FIP6Used then
  begin
    Multicast6.ipv6mr_multiaddr := StrToIp6(MCastIP).sin6_addr;
    Multicast6.ipv6mr_interface := 0;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_JOIN_GROUP,
      pchar(@Multicast6), SizeOf(Multicast6)));
  end
  else
  begin
    Multicast.imr_multiaddr.S_addr := synsock.inet_addr(PChar(MCastIP));
    Multicast.imr_interface.S_addr := u_long(INADDR_ANY);
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_ADD_MEMBERSHIP,
      pchar(@Multicast), SizeOf(Multicast)));
  end;
  ExceptCheck;
end;

procedure TUDPBlockSocket.DropMulticast(MCastIP: string);
var
  Multicast: TIP_mreq;
  Multicast6: TIPv6_mreq;
begin
  if FIP6Used then
  begin
    Multicast6.ipv6mr_multiaddr := StrToIp6(MCastIP).sin6_addr;
    Multicast6.ipv6mr_interface := 0;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_LEAVE_GROUP,
      pchar(@Multicast6), SizeOf(Multicast6)));
  end
  else
  begin
    Multicast.imr_multiaddr.S_addr := synsock.inet_addr(PChar(MCastIP));
    Multicast.imr_interface.S_addr := u_long(INADDR_ANY);
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_DROP_MEMBERSHIP,
      pchar(@Multicast), SizeOf(Multicast)));
  end;
  ExceptCheck;
end;
{$ENDIF}

procedure TUDPBlockSocket.SetMulticastTTL(TTL: integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_MulticastTTL;
  d.Value := TTL;
  DelayedOption(d);
end;

function TUDPBlockSocket.GetMulticastTTL:integer;
var
  l: Integer;
begin
{$IFNDEF CIL}
  l := SizeOf(Result);
  if FIP6Used then
    synsock.GetSockOpt(FSocket, IPPROTO_IPV6, IPV6_MULTICAST_HOPS, @Result, l)
  else
    synsock.GetSockOpt(FSocket, IPPROTO_IP, IP_MULTICAST_TTL, @Result, l);
{$ENDIF}
end;

procedure TUDPBlockSocket.EnableMulticastLoop(Value: Boolean);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_MulticastLoop;
  d.Enabled := Value;
  DelayedOption(d);
end;

function TUDPBlockSocket.GetSocketType: integer;
begin
  Result := integer(SOCK_DGRAM);
end;

function TUDPBlockSocket.GetSocketProtocol: integer;
begin
 Result := integer(IPPROTO_UDP);
end;

{======================================================================}

{$IFNDEF CIL}
function PasswordCallback(buf:PChar; size:Integer; rwflag:Integer; userdata: Pointer):Integer; cdecl;
var
  Password: String;
begin
  Password := '';
  if TTCPBlockSocket(userdata) is TTCPBlockSocket then
    Password := TTCPBlockSocket(userdata).SSLPassword;
  if Length(Password) > (Size - 1) then
    SetLength(Password, Size - 1);
  Result := Length(Password);
  StrLCopy(buf, PChar(Password + #0), Result + 1);
end;
{$ENDIF}

constructor TTCPBlockSocket.Create;
begin
  inherited Create;
  FSslEnabled := False;
  FSslBypass := False;
  FSSLCiphers := 'DEFAULT';
  FSSLCertificateFile := '';
  FSSLPrivateKeyFile := '';
  FSSLPassword  := '';
  FSsl := nil;
  Fctx := nil;
  FSSLLastError := 0;
  FSSLLastErrorDesc := '';
  FSSLverifyCert := False;
  FSSLType := LT_all;
  FHTTPTunnelIP := '';
  FHTTPTunnelPort := '';
  FHTTPTunnel := False;
  FHTTPTunnelRemoteIP := '';
  FHTTPTunnelRemotePort := '';
  FHTTPTunnelUser := '';
  FHTTPTunnelPass := '';
  FHTTPTunnelTimeout := 30000;
end;

procedure TTCPBlockSocket.CloseSocket;
begin
  if SSLEnabled then
  begin
    if assigned(FSsl) then
      sslshutdown(FSsl);
    FSSLEnabled := false;
  end;
  if FSocket <> INVALID_SOCKET then
  begin
    Synsock.Shutdown(FSocket, 1);
    Purge;
  end;
  inherited CloseSocket;
end;

function TTCPBlockSocket.WaitingData: Integer;
begin
  Result := 0;
  if FSslEnabled and not(FSslBypass) and not(FBypassFlag) then
    Result := sslpending(Fssl);
  if Result = 0 then
    Result := inherited WaitingData;
end;

procedure TTCPBlockSocket.Listen;
var
  b: Boolean;
  Sip,SPort: string;
begin
  if FSocksIP = '' then
  begin
    SockCheck(synsock.Listen(FSocket, SOMAXCONN));
    GetSins;
  end
  else
  begin
    Sip := GetLocalSinIP;
    if Sip = cAnyHost then
      Sip := LocalName;
    SPort := IntToStr(GetLocalSinPort);
    inherited Connect(FSocksIP, FSocksPort);
    b := SocksOpen;
    if b then
      b := SocksRequest(2, Sip, SPort);
    if b then
      b := SocksResponse;
    if not b and (FLastError = 0) then
      FLastError := WSANO_RECOVERY;
    FSocksLocalIP := FSocksResponseIP;
    if FSocksLocalIP = cAnyHost then
      FSocksLocalIP := FSocksIP;
    FSocksLocalPort := FSocksResponsePort;
    FSocksRemoteIP := '';
    FSocksRemotePort := '';
  end;
  ExceptCheck;
  DoStatus(HR_Listen, '');
end;

function TTCPBlockSocket.Accept: TSocket;
begin
  if FUsingSocks then
  begin
    if not SocksResponse and (FLastError = 0) then
      FLastError := WSANO_RECOVERY;
    FSocksRemoteIP := FSocksResponseIP;
    FSocksRemotePort := FSocksResponsePort;
    Result := FSocket;
  end
  else
  begin
    Result := synsock.Accept(FSocket, FRemoteSin);
///    SockCheck(Result);
  end;
  ExceptCheck;
  DoStatus(HR_Accept, '');
end;

procedure TTCPBlockSocket.Connect(IP, Port: string);
var
  x: integer;
begin
  if FSocksIP <> '' then
    SocksDoConnect(IP, Port)
  else
    if FHTTPTunnelIP <> '' then
      HTTPTunnelDoConnect(IP, Port)
    else
      inherited Connect(IP, Port);
  if FSslEnabled then
    if FLastError = 0 then
      SSLDoConnect
    else
    begin
      x := FLastError;
      SSLEnabled := False;
      FLastError := x;
    end;
end;

procedure TTCPBlockSocket.SocksDoConnect(IP, Port: string);
var
  b: Boolean;
begin
  inherited Connect(FSocksIP, FSocksPort);
  if FLastError = 0 then
  begin
    b := SocksOpen;
    if b then
      b := SocksRequest(1, IP, Port);
    if b then
      b := SocksResponse;
    if not b and (FLastError = 0) then
      FLastError := WSASYSNOTREADY;
    FSocksLocalIP := FSocksResponseIP;
    FSocksLocalPort := FSocksResponsePort;
    FSocksRemoteIP := IP;
    FSocksRemotePort := Port;
  end;
  ExceptCheck;
  DoStatus(HR_Connect, IP + ':' + Port);
end;

procedure TTCPBlockSocket.HTTPTunnelDoConnect(IP, Port: string);
//bugfixed by Mike Green (mgreen@emixode.com)
var
  s: string;
begin
  try
    Port := IntToStr(ResolvePort(Port));
    FBypassFlag := True;
    inherited Connect(FHTTPTunnelIP, FHTTPTunnelPort);
    if FLastError <> 0 then
      Exit;
    FHTTPTunnel := False;
    if IsIP6(IP) then
      IP := '[' + IP + ']';
    SendString('CONNECT ' + IP + ':' + Port + ' HTTP/1.0' + CRLF);
    if FHTTPTunnelUser <> '' then
    Sendstring('Proxy-Authorization: Basic ' +
      EncodeBase64(FHTTPTunnelUser + ':' + FHTTPTunnelPass) + CRLF);
    SendString(CRLF);
    repeat
      s := RecvTerminated(FHTTPTunnelTimeout, #$0a);
      if FLastError <> 0 then
        Break;
      if (Pos('HTTP/', s) = 1) and (Length(s) > 11) then
        FHTTPTunnel := s[10] = '2';
    until (s = '') or (s = #$0d);
    if (FLasterror = 0) and not FHTTPTunnel then
      FLastError := WSASYSNOTREADY;
    FHTTPTunnelRemoteIP := IP;
    FHTTPTunnelRemotePort := Port;
  finally
    FBypassFlag := False;
  end;
  ExceptCheck;
end;

procedure TTCPBlockSocket.SSLDoConnect;
var
  x: integer;
begin
  FLastError := 0;
  if not FSSLEnabled then
    SSLEnabled := True;
  if (FLastError = 0) then
{$IFDEF CIL}
    if sslsetfd(FSsl, FSocket.Handle.ToInt32) < 1 then
{$ELSE}
    if sslsetfd(FSsl, FSocket) < 1 then
{$ENDIF}
    begin
      FLastError := WSASYSNOTREADY;
      SSLCheck;
    end;
  if (FLastError = 0) then
  begin
    x := sslconnect(FSsl);
    if x < 1 then
    begin
      FLastError := WSASYSNOTREADY;
      SSLcheck;
    end;
  end;
  if FSSLverifyCert then
    if SSLGetVerifyCert <> 0 then
      FLastError := WSAEACCES;
  if FLastError <> 0 then
  begin
    x := FLastError;
    SSLEnabled := False;
    FLastError := x;
  end;
  ExceptCheck;
end;

procedure TTCPBlockSocket.SSLDoShutdown;
var
  x: integer;
begin
  FLastError := 0;
  if assigned(FSsl) then
  begin
    x := sslshutdown(FSsl);
    if x = 0 then
    begin
      Synsock.Shutdown(FSocket, 1);
      sslshutdown(FSsl);
    end;
  end;
  SSLEnabled := False;
  ExceptCheck;
end;

function TTCPBlockSocket.GetLocalSinIP: string;
begin
  if FUsingSocks then
    Result := FSocksLocalIP
  else
    Result := inherited GetLocalSinIP;
end;

function TTCPBlockSocket.GetRemoteSinIP: string;
begin
  if FUsingSocks then
    Result := FSocksRemoteIP
  else
    if FHTTPTunnel then
      Result := FHTTPTunnelRemoteIP
    else
      Result := inherited GetRemoteSinIP;
end;

function TTCPBlockSocket.GetLocalSinPort: Integer;
begin
  if FUsingSocks then
    Result := StrToIntDef(FSocksLocalPort, 0)
  else
    Result := inherited GetLocalSinPort;
end;

function TTCPBlockSocket.GetRemoteSinPort: Integer;
begin
  if FUsingSocks then
    Result := ResolvePort(FSocksRemotePort)
  else
    if FHTTPTunnel then
      Result := StrToIntDef(FHTTPTunnelRemotePort, 0)
    else
      Result := inherited GetRemoteSinPort;
end;

function TTCPBlockSocket.SSLCheck: Boolean;
var
  ErrBuf: String;
begin
  Result := true;
  FSSLLastErrorDesc := '';
  FSSLLastError := ErrGetError;
  ErrClearError;
  if FSSLLastError <> 0 then
  begin
    Result := False;
    ErrBuf := StringOfChar(#0, 256);
    FSSLLastErrorDesc := ErrErrorString(FSSLLastError, ErrBuf);
  end;
end;

function TTCPBlockSocket.GetSSLLoaded: Boolean;
begin
  Result := IsSSLLoaded;
end;

function TTCPBlockSocket.SetSslKeys: boolean;
begin
  if not assigned(FCtx) then
  begin
    Result := False;
    Exit;
  end
  else
    Result := True;
  if FSSLCertificateFile <> '' then
    if SslCtxUseCertificateChainFile(FCtx, FSSLCertificateFile) <> 1 then
    begin
      Result := False;
      SSLCheck;
      Exit;
    end;
  if FSSLPrivateKeyFile <> '' then
    if SslCtxUsePrivateKeyFile(FCtx, FSSLPrivateKeyFile, 1) <> 1 then
    begin
      Result := False;
      SSLCheck;
      Exit;
    end;
  if FSSLCertCAFile <> '' then
    if SslCtxLoadVerifyLocations(FCtx, FSSLCertCAFile, '') <> 1 then
    begin
      Result := False;
      SSLCheck;
    end;
end;

procedure TTCPBlockSocket.SetSslEnabled(Value: Boolean);
var
  err: Boolean;
begin
  FLastError := 0;
  if Value <> FSslEnabled then
    if Value then
    begin
      FBuffer := '';
      FSSLLastErrorDesc := '';
      FSSLLastError := 0;
      if InitSSLInterface then
      begin
        err := False;
        Fctx := nil;
        case FSSLType of
          LT_SSLv2:
            Fctx := SslCtxNew(SslMethodV2);
          LT_SSLv3:
            Fctx := SslCtxNew(SslMethodV3);
          LT_TLSv1:
            Fctx := SslCtxNew(SslMethodTLSV1);
          LT_all:
            Fctx := SslCtxNew(SslMethodV23);
        end;
        if Fctx = nil then
        begin
          SSLCheck;
          FlastError := WSAEPROTONOSUPPORT;
          err := True;
        end
        else
        begin
          SslCtxSetCipherList(Fctx, FSSLCiphers);
          if FSSLverifyCert then
            SslCtxSetVerify(FCtx, SSL_VERIFY_PEER, nil)
          else
            SslCtxSetVerify(FCtx, SSL_VERIFY_NONE, nil);
{$IFNDEF CIL}
          SslCtxSetDefaultPasswdCb(FCtx, @PasswordCallback);
          SslCtxSetDefaultPasswdCbUserdata(FCtx, self);
{$ENDIF}
          if not SetSSLKeys then
            FLastError := WSAEINVAL
          else
          begin
            Fssl := nil;
            Fssl := SslNew(Fctx);
            if Fssl = nil then
            begin
              SSLCheck;
              FlastError := WSAEPROTONOSUPPORT;
              err := True;
            end;
          end;
        end;
        FSslEnabled := not err;
      end
      else
        FlastError := WSAEPROTONOSUPPORT;
    end
    else
    begin
      FBuffer := '';
      sslfree(Fssl);
      Fssl := nil;
      SslCtxFree(Fctx);
      Fctx := nil;
      ErrRemoveState(0);
      FSslEnabled := False;
    end;
  ExceptCheck;
end;

function TTCPBlockSocket.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  err: integer;
{$IFDEF CIL}
  sb: stringbuilder;
  s: ansistring;
{$ENDIF}
begin
  if FSslEnabled and not(FSslBypass) and not(FBypassFlag) then
  begin
    Result := 0;
    if TestStopFlag then
      Exit;
    FLastError := 0;
    repeat
{$IFDEF CIL}
      sb := StringBuilder.Create(Len);
      Result := SslRead(FSsl, sb, Len);
      if Result > 0 then
      begin
        sb.Length := Result;
        s := sb.ToString;
        System.Array.Copy(BytesOf(s), Buffer, length(s));
      end;
{$ELSE}
      Result := SslRead(FSsl, Buffer , Len);
{$ENDIF}
      err := SslGetError(FSsl, Result);
    until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
    if err = SSL_ERROR_ZERO_RETURN then
      Result := 0
    else
      if (err <> 0) then
        FLastError := WSASYSNOTREADY;
    ExceptCheck;
    Inc(FRecvCounter, Result);
    DoStatus(HR_ReadCount, IntToStr(Result));
    DoReadFilter(Buffer, Result);
  end
  else
    Result := inherited RecvBuffer(Buffer, Len);
end;

function TTCPBlockSocket.SendBuffer(Buffer: TMemory; Length: Integer): Integer;
var
  err: integer;
  x, y: integer;
  l, r: integer;
{$IFDEF CIL}
  s: string;
{$ELSE}
  p: Pointer;
{$ENDIF}
begin
  if FSslEnabled and not(FSslBypass) and not(FBypassFlag) then
  begin
    Result := 0;
    if TestStopFlag then
      Exit;
    FLastError := 0;
    DoWriteFilter(Buffer, Length);
{$IFDEF CIL}
    s := StringOf(Buffer);
    repeat
      r := SslWrite(FSsl, s, Length);
      err := SslGetError(FSsl, r);
    until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
    if err = SSL_ERROR_ZERO_RETURN then
      r := 0
    else
      if (err <> 0) then
        FLastError := WSASYSNOTREADY;
    Result := r;
    Inc(FSendCounter, r);
    DoStatus(HR_WriteCount, IntToStr(r));
{$ELSE}
    l := Length;
    x := 0;
    while x < l do
    begin
      y := l - x;
      if y > FSendMaxChunk then
        y := FSendMaxChunk;
      if y > 0 then
      begin
        LimitBandwidth(y, FMaxSendBandwidth, FNextsend);
        p := IncPoint(Buffer, x);
        repeat
          r := SslWrite(FSsl, p, y);
          err := SslGetError(FSsl, r);
        until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
        if err = SSL_ERROR_ZERO_RETURN then
          r := 0
        else
          if (err <> 0) then
            FLastError := WSASYSNOTREADY;
        if Flasterror <> 0 then
          Break;
        Inc(x, r);
        Inc(Result, r);
        Inc(FSendCounter, r);
        DoStatus(HR_WriteCount, IntToStr(r));
      end
      else
        break;
    end;
{$ENDIF}
    ExceptCheck;
  end
  else
    Result := inherited SendBuffer(Buffer, Length);
end;

function TTCPBlockSocket.SSLAcceptConnection: Boolean;
var
  x: integer;
begin
  FLastError := 0;
  if not FSSLEnabled then
    SSLEnabled := True;
  if (FLastError = 0) then
  begin
{$IFDEF CIL}
    x := FSocket.Handle.ToInt32;
    if sslsetfd(FSsl, x) < 1 then
{$ELSE}
    if sslsetfd(FSsl, FSocket) < 1 then
{$ENDIF}
    begin
      FLastError := WSASYSNOTREADY;
      SSLCheck;
    end;
  end;
  if (FLastError = 0) then
    if sslAccept(FSsl) < 1 then
      FLastError := WSASYSNOTREADY;
  if FLastError <> 0 then
  begin
    x := FLastError;
    SSLEnabled := False;
    FLastError := x;
  end;
  ExceptCheck;
  Result := FLastError = 0;
end;

function TTCPBlockSocket.SSLGetSSLVersion: string;
begin
  if not assigned(FSsl) then
    Result := ''
  else
    Result := SSlGetVersion(FSsl);
end;

function TTCPBlockSocket.SSLGetPeerSubject: string;
var
  cert: PX509;
  s: string;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
{$IFDEF CIL}
  sb := StringBuilder.Create(4096);
  Result := SslX509NameOneline(SslX509GetSubjectName(cert), sb, 4096);
{$ELSE}
  setlength(s, 4096);
  Result := SslX509NameOneline(SslX509GetSubjectName(cert), s, Length(s));
{$ENDIF}
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerName: string;
var
  s: string;
begin
  s := SSLGetPeerSubject;
  s := SeparateRight(s, '/CN=');
  Result := Trim(SeparateLeft(s, '/'));
end;

function TTCPBlockSocket.SSLGetPeerIssuer: string;
var
  cert: PX509;
  s: string;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
{$IFDEF CIL}
  sb := StringBuilder.Create(4096);
  Result := SslX509NameOneline(SslX509GetIssuerName(cert), sb, 4096);
{$ELSE}
  setlength(s, 4096);
  Result := SslX509NameOneline(SslX509GetIssuerName(cert), s, Length(s));
{$ENDIF}
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerSubjectHash: Cardinal;
var
  cert: PX509;
begin
  if not assigned(FSsl) then
  begin
    Result := 0;
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  Result := SslX509NameHash(SslX509GetSubjectName(cert));
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerIssuerHash: Cardinal;
var
  cert: PX509;
begin
  if not assigned(FSsl) then
  begin
    Result := 0;
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  Result := SslX509NameHash(SslX509GetIssuerName(cert));
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerFingerprint: string;
var
  cert: PX509;
  x: integer;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
{$IFDEF CIL}
  sb := StringBuilder.Create(EVP_MAX_MD_SIZE);
  SslX509Digest(cert, SslEvpMd5, sb, x);
  sb.Length := x;
  Result := sb.ToString;
{$ELSE}
  setlength(Result, EVP_MAX_MD_SIZE);
  SslX509Digest(cert, SslEvpMd5, Result, x);
  SetLength(Result, x);
{$ENDIF}
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetCertInfo: string;
var
  cert: PX509;
  x, y: integer;
  b: PBIO;
  s: AnsiString;
{$IFDEF CIL}
  sb: stringbuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  b := BioNew(BioSMem);
  try
    X509Print(b, cert);
    x := bioctrlpending(b);
{$IFDEF CIL}
    sb := StringBuilder.Create(x);
    y := bioread(b, sb, x);
    if y > 0 then
    begin
      sb.Length := y;
      s := sb.ToString;
    end;
{$ELSE}
    setlength(s,x);
    y := bioread(b,s,x);
    if y > 0 then
      setlength(s, y);
{$ENDIF}
    Result := ReplaceString(s, LF, CRLF);
  finally
    BioFreeAll(b);
  end;
end;

function TTCPBlockSocket.SSLGetCipherName: string;
begin
  if not assigned(FSsl) then
    Result := ''
  else
    Result := SslCipherGetName(SslGetCurrentCipher(FSsl));
end;

function TTCPBlockSocket.SSLGetCipherBits: integer;
var
  x: integer;
begin
  if not assigned(FSsl) then
    Result := 0
  else
    Result := SSLCipherGetBits(SslGetCurrentCipher(FSsl), x);
end;

function TTCPBlockSocket.SSLGetCipherAlgBits: integer;
begin
  if not assigned(FSsl) then
    Result := 0
  else
    SSLCipherGetBits(SslGetCurrentCipher(FSsl), Result);
end;

function TTCPBlockSocket.SSLGetVerifyCert: integer;
begin
  if not assigned(FSsl) then
    Result := 1
  else
    Result := SslGetVerifyResult(FSsl);
end;

function TTCPBlockSocket.GetSocketType: integer;
begin
  Result := integer(SOCK_STREAM);
end;

function TTCPBlockSocket.GetSocketProtocol: integer;
begin
  Result := integer(IPPROTO_TCP);
end;

{======================================================================}

function TICMPBlockSocket.GetSocketType: integer;
begin
  Result := integer(SOCK_RAW);
end;

function TICMPBlockSocket.GetSocketProtocol: integer;
begin
  if FIP6Used then
    Result := integer(IPPROTO_ICMPV6)
  else
    Result := integer(IPPROTO_ICMP);
end;

{======================================================================}

function TRAWBlockSocket.GetSocketType: integer;
begin
  Result := integer(SOCK_RAW);
end;

function TRAWBlockSocket.GetSocketProtocol: integer;
begin
  Result := integer(IPPROTO_RAW);
end;

{======================================================================}

constructor TSynaClient.Create;
begin
  inherited Create;
  FIPInterface := cAnyHost;
  FTargetHost := cLocalhost;
  FTargetPort := cAnyPort;
  FTimeout := 5000;
  FUsername := '';
  FPassword := '';
end;

{======================================================================}

{$IFDEF ONCEWINSOCK}
initialization
begin
  if not InitSocketInterface(DLLStackName) then
  begin
    e := ESynapseError.Create('Error loading Socket interface (' + DLLStackName + ')!');
    e.ErrorCode := 0;
    e.ErrorMessage := 'Error loading Socket interface (' + DLLStackName + ')!';
    raise e;
  end;
  synsock.WSAStartup(WinsockLevel, WsaDataOnce);
end;
{$ENDIF}

finalization
begin
{$IFDEF ONCEWINSOCK}
  synsock.WSACleanup;
  DestroySocketInterface;
{$ENDIF}
end;

end.
