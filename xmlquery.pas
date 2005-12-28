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
     CustomXMLDataset, CustomSQLConn;

type

  { TCustomXMLQuery - adds sql states execution to the dataset and uses a connection }

  { TCustomXMLQuery }

  TCustomXMLQuery = class(TCustomXMLDataSet)
  private
    FSQL : TStrings;
    FSQLXML : TXMLDocument; // XML document used to pass sql statements to connection
    FSQLConnection : TCustomSQLConnection; // a connection to retreive XML / execute SQL
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
  end;
  
implementation

{$DEFINE DEBUGXML}

uses uXMLDSConsts, XMLRead
     {$IFDEF DEBUGXML}
     , XMLWrite
     {$ENDIF}
     ;

(*******************************************************************************
{ TCustomXMLQuery }
*******************************************************************************)

procedure TCustomXMLQuery.SetSQL(const AValue: TStrings);
begin
  Close;
  SQL.BeginUpdate;
  try
    SQL.Assign(AValue);
  finally
    SQL.EndUpdate;
  end;
end;

function TCustomXMLQuery.GetSQLXMLStringStream: TStringStream;
var strm : TStringStream;
    S : String;
begin
  try
    strm   := TStringStream.Create('');
    WriteXML(FSQLXML.DocumentElement, strm);
    S := '<?xml version="1.0" encoding="'+TO_ENCODING+'"?>'+strm.DataString;
  finally
    strm.Free;
// todo : fix memory leaks
    Result := TStringStream.Create(S);
  end;
end;

procedure TCustomXMLQuery.SetSQLConnection(const AValue: TCustomSQLConnection);
begin
  CheckInactive;
  if Assigned(FSQLConnection) then
     FSQLConnection.UnRegisterClient(Self);
  FSQLConnection := AValue;
  if Assigned(FSQLConnection) then
     FSQLConnection.RegisterClient(Self,nil);
end;

procedure TCustomXMLQuery.ConstructQuery(const QueryType : String);
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

constructor TCustomXMLQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FSQLXML := TXMLDocument.Create;
end;

destructor TCustomXMLQuery.Destroy;
begin
  SetSQLConnection(nil);
  FSQL.Free;
  FSQLXML.Free;
  inherited Destroy;
end;

procedure TCustomXMLQuery.ExecSQL(const QueryType : String);
begin
  ConstructQuery(QueryType);
  FSQLConnection.DataToSend := SQLXMLStringStream; // send current sql xml
//todo : check if this is working
  if FSQLConnection.DataToSend.Size = 0 then exit;
  
  if not FSQLConnection.Open then
     raise Exception.Create('TCustomXMLQuery.ExecSQL - connection failed!');
     
  if (QueryType = QUERY_SELECT) then // and get a new one
    begin
      Close;
      ReadXMLFile(XMLDocument,FSQLConnection.ReceivedData);
      Open; // reopen dataset
    end;
end;

end.

