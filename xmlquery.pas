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
  Classes, SysUtils, BaseXMLDataset, SQLConnection;

type

  { TBaseXMLQuery - adds sql states execution to the dataset and uses a connection }
  TBaseXMLQuery = class(TBaseXMLDataSet)
  private
    FSQL : TStrings;
    FSQLConnection : TBaseSQLConnection; // a connection to retreive XML / execute SQL
    procedure SetSQL(const AValue: TStrings);
    procedure SetSQLConnection(const AValue: TBaseSQLConnection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
/////////
//    procedure Open; override;  // SELECT only
//    procedure ExecSQL;         // INSERT, DELETE, MODIFY, CREATE TABLE, etc ...
  published
    property SQL : TStrings read FSQL write SetSQL;
    property Connection : TBaseSQLConnection read FSQLConnection write SetSQLConnection;
  end;
  
implementation


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

constructor TBaseXMLQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
end;

destructor TBaseXMLQuery.Destroy;
begin
  SetSQLConnection(nil);
  FSQL.Free;
  inherited Destroy;
end;

end.

