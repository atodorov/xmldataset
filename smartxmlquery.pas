unit smartxmlquery;

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
     XMLQuery, CustomXMLDataset;
type

  {
    TSmartXMLQuery - has capability of parsing datapackets and builds
    sql_xml packets for every record in the dataset. Use this to save overhead
    at the other side of the connection (e.g. cgi script at a web server).
  }
  TSmartXMLQuery = class(TCustomXMLQuery)
  protected
    { Constructs sql statements that are saved into XMLDocument instead of standard
      datapacket.xml contents. Everything is sent over the connection with commit.
      SLQ is constructed when at a BeforeComit event handler.
      The statements are in the following order :
      INSERT's, UPDATE's, DELETE's
    }
    procedure SQLConnBeforeCommitDataset(Sender : TObject; Dataset : TCustomXMLDataSet);
    procedure ConstructSQLFromXML; virtual;
    function CreateInsertFromNode(const ANode : TDOMElement; AOwner : TXMLDocument) : TDOMElement;
    function CreateUpdateFromNode(const ANode : TDOMElement; AOwner : TXMLDocument) : TDOMElement;
    function CreateDeleteFromNode(const ANode : TDOMElement; AOwner : TXMLDocument) : TDOMElement;
  public
    constructor Create(AOwner : TComponent); override;
  end;

implementation

uses uXMLDSConsts;

{ TSmartXMLQuery }

constructor TSmartXMLQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Connection.BeforeCommitDataset := @SQLConnBeforeCommitDataset;
end;

procedure TSmartXMLQuery.SQLConnBeforeCommitDataset(Sender: TObject; Dataset: TCustomXMLDataSet);
begin
  if (Dataset is TSmartXMLQuery) then
     TSmartXMLQuery(Dataset).ConstructSQLFromXML;
end;

procedure TSmartXMLQuery.ConstructSQLFromXML;
var LNode, LInserted, LModified, LDeleted : TDOMElement;
    LDoc  : TXMLDocument;
    i : LongWord;
begin
  try
    LDoc := TXMLDocument.Create;

    LNode := LDoc.CreateElement(cDocument);
    LNode.AttribStrings[cDocument_Type] := cDocument_Type_SQL;
    LDoc.AppendChild(LNode);

    LInserted := LDoc.CreateElement(cQuery);
    LInserted.AttribStrings[cQuery_Type] := QUERY_INSERT;
    
    LModified := LDoc.CreateElement(cQuery);
    LModified.AttribStrings[cQuery_Type] := QUERY_UPDATE;
    
    LDeleted := LDoc.CreateElement(cQuery);
    LDeleted.AttribStrings[cQuery_Type] := QUERY_DELETE;
    
    for i := 0 to XMLDocument.DocumentElement.FindNode(cRecordData).ChildNodes.Count - 1 do
      begin
        LNode := TDOMElement(XMLDocument.DocumentElement.FindNode(cRecordData).ChildNodes.Item[i]);
        case StrToInt(LNode.AttribStrings[cRow_State]) of
          ROW_INSERTED, ROW_INSERTED and ROW_MODIFIED :
              LInserted.AppendChild(CreateInsertFromNode(LNode, LDoc));
          ROW_MODIFIED : LModified.AppendChild(CreateUpdateFromNode(LNode, LDoc));
        end;
      end;

    // handle deleted records
    for i := 0 to XMLDocument.DocumentElement.FindNode(cDeletedRecords).ChildNodes.Count - 1 do
      LDeleted.AppendChild(
        CreateDeleteFromNode(
          TDOMElement(XMLDocument.DocumentElement.FindNode(cDeletedRecords).ChildNodes.Item[i]),
          LDoc));
          
    // assign new XML document
    XMLDocument := LDoc;
    
  finally
    LDoc  := nil;
    LNode := nil; // clear referecne
    LInserted := nil;
    LModified := nil;
    LDeleted  := nil;
  end;
end;

function TSmartXMLQuery.CreateInsertFromNode(const ANode : TDOMElement; AOwner : TXMLDocument) : TDOMElement;
var CData : TDOMCDATASection;
    strSQL, strFields, strValues : String;
    i : LongWord;
begin
  try
    Result := AOwner.CreateElement(cQuery);
    Result.AttribStrings[cQuery_Type] := QUERY_INSERT;
    strSQL := 'INSERT INTO ' +
      TDOMElement(ANode.OwnerDocument.DocumentElement.FindNode(cMetadata).FindNode(cTable)).AttribStrings[cTable_Name] +
      '( ';

    strFields := '';
    strValues := '';
    
    for i := 0 to ANode.ChildNodes.Count - 1 do
      begin
        strFields := strFields + ' ' + TDOMElement(ANode.ChildNodes.Item[i]).AttribStrings[cField_Name] + ',';

//todo : base64Decoding, N.B. BLOBS, N.B. quotes
        strValues := strValues + ' "' +
           TDOMElement(ANode.ChildNodes.Item[i]).AttribStrings[cField_Value] + '",';
      end;

    // strip trailing ,
    strSQL := Copy(strFields,1,Length(strFields)-1) + ') VALUES ( '+
              Copy(strValues,1,Length(strValues)-1) + ')';

    CData := AOwner.CreateCDATASection(strSQL);
    Result.AppendChild(CData);
  finally
    CData := nil; // clear reference
  end;
end;

function TSmartXMLQuery.CreateUpdateFromNode(const ANode : TDOMElement; AOwner : TXMLDocument) : TDOMElement;
var CData : TDOMCDATASection;
    strSQL : String;
    i : LongWord;
begin
  try
    Result := AOwner.CreateElement(cQuery);
    Result.AttribStrings[cQuery_Type] := QUERY_UPDATE;
    strSQL := 'UPDATE ' +
      TDOMElement(ANode.OwnerDocument.DocumentElement.FindNode(cMetadata).FindNode(cTable)).AttribStrings[cTable_Name] +
      ' SET ';

    for i := 0 to ANode.ChildNodes.Count - 1 do
//todo : base64Decoding, N.B. BLOBS, N.B. quotes
        strSQL := strSQL + ' ' +
           TDOMElement(ANode.ChildNodes.Item[i]).AttribStrings[cField_Name] + '=' +
           ' "' + TDOMElement(ANode.ChildNodes.Item[i]).AttribStrings[cField_Value] + '",';

    // strip trailing ,
    strSQL := Copy(strSQL,1,Length(strSQL)-1) + ' WHERE ';
//TODO : IMPLEMENT WHERE KEYS

    CData := AOwner.CreateCDATASection(strSQL);
    Result.AppendChild(CData);
  finally
    CData := nil; // clear reference
  end;
end;

function TSmartXMLQuery.CreateDeleteFromNode(const ANode : TDOMElement; AOwner : TXMLDocument) : TDOMElement;
var CData : TDOMCDATASection;
    strSQL : String;
    i : LongWord;
begin
  try
    Result := AOwner.CreateElement(cQuery);
    Result.AttribStrings[cQuery_Type] := QUERY_DELETE;
    strSQL := 'DELETE FROM ' +
      TDOMElement(ANode.OwnerDocument.DocumentElement.FindNode(cMetadata).FindNode(cTable)).AttribStrings[cTable_Name] +
      ' WHERE ';
//TODO : IMPLEMENT WHERE KEYS

(*
    for i := 0 to ANode.ChildNodes.Count - 1 do
//todo : base64Decoding, N.B. BLOBS, N.B. quotes
        strSQL := strSQL + ' ' +
           TDOMElement(ANode.ChildNodes.Item[i]).AttribStrings[cField_Name] + '=' +
           ' "' + TDOMElement(ANode.ChildNodes.Item[i]).AttribStrings[cField_Value] + '",';

    // strip trailing ,
    strSQL := Copy(strSQL,1,Length(strSQL)-1) + ' WHERE ';
*)

    CData := AOwner.CreateCDATASection(strSQL);
    Result.AppendChild(CData);
  finally
    CData := nil; // clear reference
  end;
end;

end.

