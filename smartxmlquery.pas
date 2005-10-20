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

//todo list
// - convert all data back to normal when constructing sql statements / Base64Decode
// - how to handle BLOB fields
// - should QuotedStr be used for string / all field values

uses Classes, SysUtils, DOM,
     XMLQuery, CustomXMLDataset, CustomSQLConn, uXMLDSConsts;
type

  {
    TSmartXMLQuery - has capability of parsing datapackets and builds
    sql_xml packets for every record in the dataset. Use this to save overhead
    at the other side of the connection (e.g. cgi script at a web server).
  }
  TSmartXMLQuery = class(TCustomXMLQuery)
  private
    FQuoteChar : Char;
    function GetUpdateMode: TUpdateMode;
  protected
    { Constructs sql statements that are saved into XMLDocument instead of standard
      datapacket.xml contents. Everything is sent over the connection with commit.
      SLQ is constructed when at a BeforeComit event handler.
      The statements are in the following order :
      INSERT's, UPDATE's, DELETE's
    }
    procedure SetSQLConnection(const AValue: TCustomSQLConnection); override;
  public
// todo : debug only
    procedure SQLConnBeforeCommitDataset(Sender : TObject; Dataset : TCustomXMLDataSet);

  protected
    procedure ConstructSQLFromXML; virtual;
    function ConstructWhereClause(const ARow :TDOMElement) : String;
    function CreateInsertFromNode(const ANode : TDOMElement; AOwner : TXMLDocument) : TDOMElement;
    function CreateUpdateFromNode(const ANode : TDOMElement; AOwner : TXMLDocument) : TDOMElement;
    function CreateDeleteFromNode(const ANode : TDOMElement; AOwner : TXMLDocument) : TDOMElement;

    property UpdateMode : TUpdateMode read GetUpdateMode;
  public
    constructor Create(AOwner : TComponent); override;
    { Character used to quote field values. Default is single quote like Firebird }
    property QuoteChar : Char read FQuoteChar write FQuoteChar;
  end;

implementation

//todo : debug only - remove
uses XMLWrite;

{ TSmartXMLQuery }

function TSmartXMLQuery.GetUpdateMode: TUpdateMode;
begin
  Result := TUpdateMode(StrToInt(
            XMLDocument.DocumentElement.FindNode(cMetadata).FindNode(cUpdateMode).
            Attributes.GetNamedItem(cUpdateMode_Value).NodeValue));
end;

procedure TSmartXMLQuery.SetSQLConnection(const AValue: TCustomSQLConnection);
begin
  inherited SetSQLConnection(AValue);
  if (Connection <> nil) then
     Connection.BeforeCommitDataset := @Self.SQLConnBeforeCommitDataset;
end;

procedure TSmartXMLQuery.SQLConnBeforeCommitDataset(Sender: TObject; Dataset: TCustomXMLDataSet);
begin
  if (Dataset is TSmartXMLQuery) then
     TSmartXMLQuery(Dataset).ConstructSQLFromXML;
//todo - debug only
  WriteXMLFile(Dataset.XMLDocument,ExtractFilePath(ParamStr(0))+'smart.xml');
end;

procedure TSmartXMLQuery.ConstructSQLFromXML;
var LNode : TDOMElement;
    LInserted, LModified, LDeleted : TList;
    LDoc  : TXMLDocument;
    i : LongWord;
begin
  try
    LDoc := TXMLDocument.Create;

    LNode := LDoc.CreateElement(cDocument);
    LNode.AttribStrings[cDocument_Type] := cDocument_Type_SQL;
    LDoc.AppendChild(LNode);

    LInserted := TList.Create;
    LModified := TList.Create;
    LDeleted  := TList.Create;

    if (XMLDocument.DocumentElement.FindNode(cRecordData)<> nil) then
      for i := 0 to XMLDocument.DocumentElement.FindNode(cRecordData).ChildNodes.Count - 1 do
        begin
          LNode := TDOMElement(XMLDocument.DocumentElement.FindNode(cRecordData).ChildNodes.Item[i]);
          case StrToInt(LNode.AttribStrings[cRow_State]) of
            ROW_INSERTED, ROW_INSERTED or ROW_MODIFIED :
                LInserted.Add(CreateInsertFromNode(LNode, LDoc));
            ROW_MODIFIED : LModified.Add(CreateUpdateFromNode(LNode, LDoc));
          end;
        end;

    // handle deleted records
    if (XMLDocument.DocumentElement.FindNode(cDeletedRecords) <> nil) then
      for i := 0 to XMLDocument.DocumentElement.FindNode(cDeletedRecords).ChildNodes.Count - 1 do
        LDeleted.Add(
          CreateDeleteFromNode(
            TDOMElement(XMLDocument.DocumentElement.FindNode(cDeletedRecords).ChildNodes.Item[i]),
            LDoc));

// add nodes in correct order
    if (LInserted.Count > 0) then
      for i := 0 to LInserted.Count - 1 do
        LDoc.DocumentElement.AppendChild(TDOMElement(LInserted.Items[i]));

    if (LModified.Count > 0) then
      for i := 0 to LModified.Count - 1 do
        LDoc.DocumentElement.AppendChild(TDOMElement(LModified.Items[i]));

    if (LDeleted.Count > 0) then
      for i := 0 to LDeleted.Count - 1 do
        LDoc.DocumentElement.AppendChild(TDOMElement(LDeleted.Items[i]));
      
    // assign new XML document
    XMLDocument := LDoc;
    
  finally
    LDoc  := nil;
    LNode := nil; // clear referecne
    LInserted.Free;
    LModified.Free;
    LDeleted.Free;
  end;
end;

function TSmartXMLQuery.ConstructWhereClause(const ARow :TDOMElement) : String;
// todo : AttribStrings['oldvalue', Def = 'value']
var i : LongWord;
begin
  Result := '';

  case UpdateMode of
   umWHERE_ALL      :
     begin
       Result := ' (' + TDOMElement(ARow.ChildNodes.Item[0]).AttribStrings[cField_Name] +
                 '='+ QuoteChar + TDOMElement(ARow.ChildNodes.Item[0]).AttribStrings[cField_Value] + QuoteChar +')';
       for i := 1 to ARow.ChildNodes.Count - 1 do;
         Result := Result + ' AND (' + TDOMElement(ARow.ChildNodes.Item[0]).AttribStrings[cField_Name] +
                   '='+ QuoteChar + TDOMElement(ARow.ChildNodes.Item[0]).AttribStrings[cField_Value] + QuoteChar +')';
     end;
   umWHERE_CHANGED  :
     begin
       Result := ' (' + TDOMElement(ARow.ChildNodes.Item[0]).AttribStrings[cField_Name] +
                 '='+ QuoteChar + TDOMElement(ARow.ChildNodes.Item[0]).AttribStrings[cField_OldValue] + QuoteChar +')';
       for i := 1 to ARow.ChildNodes.Count - 1 do;
         Result := Result + ' AND (' + TDOMElement(ARow.ChildNodes.Item[0]).AttribStrings[cField_Name] +
                   '='+ QuoteChar + TDOMElement(ARow.ChildNodes.Item[0]).AttribStrings[cField_OldValue] + QuoteChar + ')';
     end;
   umWHERE_KEY_ONLY :
     begin // find primary key
       Result := ' ' + XMLDocument.DocumentElement.FindNode(cMetadata).FindNode(cConstraints).FindNode(cKeys).
                 FindNode(cPrimaryKey).Attributes.GetNamedItem(cPrimaryKey_Field).NodeValue;
       Result := Result + '='+ QuoteChar + GetFieldNodeByName(ARow, Result).AttribStrings[cField_Value] + QuoteChar;
     end
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
      ' (';

    strFields := '';
    strValues := '';
    
    for i := 0 to ANode.ChildNodes.Count - 1 do
      begin
        strFields := strFields + ' ' + TDOMElement(ANode.ChildNodes.Item[i]).AttribStrings[cField_Name] + ',';

//todo : base64Decoding, N.B. BLOBS, N.B. quotes
        strValues := strValues + ' ' + QuoteChar +
           TDOMElement(ANode.ChildNodes.Item[i]).AttribStrings[cField_Value] + QuoteChar + ',';
      end;

    // strip trailing ,
    strSQL := strSQL + Copy(strFields,1,Length(strFields)-1) + ') VALUES ('+
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
      ' SET';

    for i := 0 to ANode.ChildNodes.Count - 1 do
//todo : base64Decoding, N.B. BLOBS, N.B. quotes
        strSQL := strSQL + ' ' +
           TDOMElement(ANode.ChildNodes.Item[i]).AttribStrings[cField_Name] + '=' +
           QuoteChar + TDOMElement(ANode.ChildNodes.Item[i]).AttribStrings[cField_Value] + QuoteChar + ',';

    // strip trailing ,
    strSQL := Copy(strSQL,1,Length(strSQL)-1) + ' WHERE';
    strSQL := strSQL + ConstructWhereClause(ANode);

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
    strSQL := strSQL + ConstructWhereClause(ANode);

    CData := AOwner.CreateCDATASection(strSQL);
    Result.AppendChild(CData);
  finally
    CData := nil; // clear reference
  end;
end;

constructor TSmartXMLQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQuoteChar := '''';
end;

end.

