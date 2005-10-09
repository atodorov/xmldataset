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

uses
  Classes, SysUtils, DOM, BaseXMLDataset, SQLConnection;

type

  { TBaseXMLQuery - adds sql states execution to the dataset and uses a connection }
  TBaseXMLQuery = class(TBaseXMLDataSet)
  private
    FSQL : TStrings;
    FSQLXML : TXMLDocument; // XML document used to pass sql statements to connection
    FSQLConnection : TBaseSQLConnection; // a connection to retreive XML / execute SQL
    procedure SetSQL(const AValue: TStrings);
    procedure SetSQLConnection(const AValue: TBaseSQLConnection);
  protected
    procedure ConstructQuery(const QueryType : String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ExecSQL(const QueryType : String); // executes FSQL
  published
    property SQL : TStrings read FSQL write SetSQL;
    property Connection : TBaseSQLConnection read FSQLConnection write SetSQLConnection;
  end;
  
implementation

{$DEFINE DEBUGXML}

uses uXMLDSConsts, XMLRead
     {$IFDEF DEBUGXML}
     , XMLWrite
     {$ENDIF}
     ;

(*******************************************************************************
{ TBaseXMLQuery }
*******************************************************************************)

procedure TBaseXMLQuery.SetSQL(const AValue: TStrings);
begin
  Close;
  SQL.BeginUpdate;
  try
    SQL.Assign(AValue);
  finally
    SQL.EndUpdate;
  end;
end;

procedure TBaseXMLQuery.SetSQLConnection(const AValue: TBaseSQLConnection);
begin
  CheckInactive;
  if Assigned(FSQLConnection) then
     FSQLConnection.UnRegisterClient(Self);
  FSQLConnection := AValue;
  if Assigned(FSQLConnection) then
     FSQLConnection.RegisterClient(Self,nil);
end;

procedure TBaseXMLQuery.ConstructQuery(const QueryType : String);
var FNode : TDOMElement;
    CDATA : TDOMCDATASection;
begin
  try
    if Assigned(FSQLXML.DocumentElement) then        // remove previous contents
       FSQLXML.RemoveChild(FSQLXML.DocumentElement); // only one SQL statement at a time
       
    FNode := FSQLXML.CreateElement(cQueryDocument);
    FSQLXML.AppendChild(FNode);

    CDATA := FSQLXML.CreateCDATASection(FSQL.Text);
    
    FNode := FSQLXML.CreateElement(cQuery);
    FNode.AttribStrings[cQuery_Type] := QueryType;
    FNode.AppendChild(CDATA);

    FSQLXML.DocumentElement.AppendChild(FNode);
  finally
    FNode := nil; // clear referecne
    CDATA := nil;
  end;
  {$IFDEF DEBUGXML}
     WriteXMLFile(FSQLXML,'query.xml');
  {$ENDIF}
end;

constructor TBaseXMLQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FSQLXML := TXMLDocument.Create;
end;

destructor TBaseXMLQuery.Destroy;
begin
  SetSQLConnection(nil);
  FSQL.Free;
  FSQLXML.Free;
  inherited Destroy;
end;

procedure TBaseXMLQuery.ExecSQL(const QueryType : String);
begin
  ConstructQuery(QueryType);
  FSQLConnection.DataToSend := XMLStringStream;   // send current xml
  FSQLConnection.Open;
  if (QueryType = QUERY_SELECT) then // and get a new one
    ReadXMLFile(XMLDocument,FSQLConnection.ReceivedData);
end;

end.

