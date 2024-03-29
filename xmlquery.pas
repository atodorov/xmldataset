unit xmlquery;

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

uses Classes, SysUtils, DOM,
     XMLDataset, CustomSQLConn;

type

  { TXMLQuery }
  
  { adds sql states execution to the dataset and uses a connection }

  TXMLQuery = class(TXMLDataSet)
  private
    FSQL : TStrings;
    FSQLXML : TXMLDocument; // XML document used to pass sql statements to connection
    FSQLConnection : TCustomSQLConnection; // a connection to retreive XML / execute SQL
    // wheather or not this query takes part in transactions. FSQLConnection may be assigned
    FBewareOfTransactions : Boolean; // and be used only for transportation media without transaction handling
    function GetSQLXMLStringStream: TStringStream;
    procedure SetSQL(const AValue: TStrings);
  protected
    procedure SetSQLConnection(const AValue: TCustomSQLConnection); virtual;
    procedure ConstructQuery(const QueryType : String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ExecSQL(const QueryType : String); // executes FSQL
  published
    property SQL : TStrings read FSQL write SetSQL;
    property SQLXMLStringStream : TStringStream read GetSQLXMLStringStream;
    property Connection : TCustomSQLConnection read FSQLConnection write SetSQLConnection;
    property BewareOfTransactions : Boolean read FBewareOfTransactions write FBewareOfTransactions default true;
  end;
  
implementation

//{$DEFINE DEBUGXML}

uses uXMLDSConsts, XMLRead, XMLWrite;

(*******************************************************************************
{ TXMLQuery }
*******************************************************************************)

procedure TXMLQuery.SetSQL(const AValue: TStrings);
begin
  Close;
  SQL.BeginUpdate;
  try
    SQL.Assign(AValue);
  finally
    SQL.EndUpdate;
  end;
end;

function TXMLQuery.GetSQLXMLStringStream: TStringStream;
var strm : TStringStream;
    S : String;
begin
  try
    strm   := TStringStream.Create('');
    WriteXML(FSQLXML.DocumentElement, strm);
    S := '<?xml version="1.0"';
    if UseCharacterEncoding then
       S := S + ' encoding="'+TO_ENCODING+'"';
    S := S + ' ?>'+strm.DataString;
  finally
    strm.Free;
// todo : fix memory leaks / or not ???
    Result := TStringStream.Create(S);
  end;
end;

procedure TXMLQuery.SetSQLConnection(const AValue: TCustomSQLConnection);
begin
  CheckInactive;
  if Assigned(FSQLConnection) then
     FSQLConnection.UnRegisterClient(Self);
  FSQLConnection := AValue;
  if Assigned(FSQLConnection) then
     FSQLConnection.RegisterClient(Self, nil);
end;

procedure TXMLQuery.ConstructQuery(const QueryType : String);
var LNode : TDOMElement;
    CDATA : TDOMCDATASection;
begin
  try
    if Assigned(FSQLXML.DocumentElement) then        // remove previous contents
       FSQLXML.RemoveChild(FSQLXML.DocumentElement);
       
    LNode := FSQLXML.CreateElement(cDocument);
    LNode.AttribStrings[cDocument_Type] := cDocument_Type_SQL;
    FSQLXML.AppendChild(LNode);

    CDATA := FSQLXML.CreateCDATASection(FSQL.Text);
    
    LNode := FSQLXML.CreateElement(cQuery);
    LNode.AttribStrings[cQuery_Type] := QueryType;
    LNode.AppendChild(CDATA);

    FSQLXML.DocumentElement.AppendChild(LNode);
  finally
    LNode := nil; // clear referecne
    CDATA := nil;
  end;
end;

constructor TXMLQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FSQLXML := TXMLDocument.Create;
  FBewareOfTransactions := true;
end;

destructor TXMLQuery.Destroy;
begin
  SetSQLConnection(nil);
  FSQL.Free;
  FSQLXML.Free;
  inherited Destroy;
end;

procedure TXMLQuery.ExecSQL(const QueryType : String);
var ssResult : TStringStream;
begin
  ConstructQuery(QueryType);
  FSQLConnection.DataToSend := SQLXMLStringStream; // send current sql xml
//todo : check if this is working
  if FSQLConnection.DataToSend.Size = 0 then exit;
  
  try
     ssResult := TStringStream.Create('');
     FSQLConnection.ReceivedData := ssResult;
     if not FSQLConnection.Open then
        raise Exception.Create('TXMLQuery.ExecSQL - connection failed!');

     if (QueryType = QUERY_SELECT) then // and get a new one
       begin
         Close;
         ReadXMLFile(XMLDocument, FSQLConnection.ReceivedData);
         Open; // reopen dataset
       end;
  finally
     ssResult.Free;
  end;
end;

end.

