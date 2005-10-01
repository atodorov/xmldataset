unit basexmldataset;

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

// a work arround widestrings. We use this because DOMString = WideString
{$DEFINE USEWIDESTRINGS}

{$DEFINE DEBUGXML}

interface

uses
  Classes, SysUtils, DB, DOM, gxBaseDataset
  {$IFDEF DEBUGXML}
    ,StdCtrls;
    var Memo : TMemo;
  {$ELSE}
  ;
  {$ENDIF}

type
////// TODO LIST
// - implement Base64 encoding for string and blob fields (for all = little dirty)
// - add display format for Float and DateTime fields. all variantions (Date, Time, Float, Currency)
// - when deleting, editing more than once, inserting, canceling => N.B. internal ID
// - add some events to Dataset / Query
// - change all Integer to Longword / Longint N.B. sign / range

  { TBaseXMLDataSet - dataset that works with XML instead a database }
  TBaseXMLDataSet=class(TGXBaseDataset)
  private
    FCurRec: Integer;
    FReadOnly: Boolean;
    FXMLDoc : TXMLDocument;
    FNode : TDOMElement;
    { internal counters }
    FDeletedCount : Longint;
    FInsertedCount : Longint;
    FModifiedCount : Longint;
    FInternalID : Longint;
    procedure SetReadOnly(Value: Boolean);
    procedure SetXMLDoc(const AValue: TXMLDocument);
  protected {Simplified Dataset methods}
    function  DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoDeleteRecord; override;
    procedure DoCreateFieldDefs; override;
    function  GetFieldValue(Field: TField): Variant; override;
    procedure SetFieldValue(Field: TField; Value: Variant); override;
    procedure GetBlobField(Field: TField; Stream: TStream); override;
    procedure SetBlobField(Field: TField; Stream: TStream); override;
    procedure DoFirst; override;
    procedure DoLast; override;
    function  Navigate(GetMode: TGetMode): TGetResult; override;
    //Record ID functions
    function  AllocateRecordID: Pointer; override;
    procedure DisposeRecordID(Value: Pointer); override;
    procedure GotoRecordID(Value: Pointer); override;
    //Bookmark functions
    function  GetBookMarkSize: Integer; override;
    procedure AllocateBookMark(RecordID: Pointer; ABookmark: Pointer); override;
    procedure DoGotoBookmark(ABookmark: Pointer); override;
    //Others
    procedure DoBeforeGetFieldValue; override;
    procedure DoAfterGetFieldValue; override;
    procedure DoBeforeSetFieldValue(Inserting: Boolean); override;
    procedure DoAfterSetFieldValue(Inserting: Boolean); override;
    {Overriden datatset methods}
    function  GetCanModify: Boolean; override;
    function  GetRecordCount: Integer; override;
    function  GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    { xml structure handling methods }
    { general section management }
    procedure AddXMLRecordToSection(const ANode: TDOMElement; const ASection : String);
    { deleting }
    procedure DeleteRecordFromXML(const ANode : TDOMElement);
    { insert / edit }
    function  CreateRowWithFields(const AState : Longint) : TDOMElement;
    { internal ID management }
    function  nGetNextID : Longint;
    function  sGetNextID : String;
    procedure AssignInternalIDS;
    { search and indexing }
    function  FindRowIndexInSection(const ARow : TDOMElement; const ASection : String) : Longint;
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(AOwner: TComponent; AXMLDoc : TXMLDocument); virtual; overload;
    constructor Create(AOwner: TComponent; AXML : String); virtual; overload;
    destructor Destroy; override;
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property XMLDocument : TXMLDocument read FXMLDoc write SetXMLDoc;
  end;

  { TBaseSQLConnection - connection and transaction handling }
  
  TLoginEvent = procedure(Sender: TObject; Username, Password: string) of object;
  TConnectChangeEvent = procedure(Sender: TObject; Connecting: Boolean) of object;

  TBaseSQLConnection = class(TComponent)
  private
    FInTransaction : Boolean; // transaction handling
    FClients: TList;
    FDataSets: TList;
    FConnectEvents: TList;
    FLoginPrompt: Boolean;
    FStreamedConnected: Boolean;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FOnLogin: TLoginEvent;
  protected
    procedure DoConnect; virtual;             // descendants must override this
    procedure DoDisconnect; virtual;          // descendants must override this
    function  GetConnected: Boolean; virtual; // descendants must override this
    function  GetDataSet(Index: Integer): TBaseXMLDataSet; virtual;
    function  GetDataSetCount: Integer; virtual;
    procedure Loaded; override;
    procedure RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil); virtual;
    procedure SetConnected(Value: Boolean); virtual;
    procedure SendConnectEvent(Connecting: Boolean);
    property  StreamedConnected: Boolean read FStreamedConnected write FStreamedConnected;
    procedure UnRegisterClient(Client: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open; overload;
    procedure Close;
    property Connected: Boolean read GetConnected write SetConnected default False;
    property DataSets[Index: Integer]: TBaseXMLDataSet read GetDataSet;
    property DataSetCount: Integer read GetDataSetCount;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt default False;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
  end;
  
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
  
  { helper functions }
  function GetFieldTypeFromString(FieldType : String) : TFieldType;
  function GetFieldSizeByType(const FieldType : TFieldType; const Size : Integer = 0) : Integer;
  function GetFieldNodeByName(const AParent : TDOMElement; AFieldName : String) : TDOMElement;
  
implementation

uses uXMLDSConsts, Variants;

{$IFDEF DEBUGXML}
procedure Log(const Msg : String);
begin
  Memo.Lines.Add(DateTimeToStr(Now)+' : '+Msg);
end;
{$ENDIF}

(*******************************************************************************
{ helper functions }
*******************************************************************************)

function GetFieldTypeFromString(FieldType : String) : TFieldType;
begin
   FieldType := AnsiUpperCase(FieldType);
   Result := ftUnknown;
        if (FieldType = 'UNKNOWN')     then Result := ftUnknown
   else if (FieldType = 'STRING')      then Result := ftString
   else if (FieldType = 'SMALLINT')    then Result := ftSmallint
   else if (FieldType = 'INTEGER')     then Result := ftInteger
   else if (FieldType = 'WORD')        then Result := ftWord
   else if (FieldType = 'BOOLEAN')     then Result := ftBoolean
   else if (FieldType = 'FLOAT')       then Result := ftFloat
   else if (FieldType = 'CURRENCY')    then Result := ftCurrency
   else if (FieldType = 'BCD')         then Result := ftBCD
   else if (FieldType = 'DATE')        then Result := ftDate
   else if (FieldType = 'TIME')        then Result := ftTime
   else if (FieldType = 'DATETIME')    then Result := ftDateTime
   else if (FieldType = 'BYTES')       then Result := ftBytes
   else if (FieldType = 'VARBYTES')    then Result := ftVarBytes
   else if (FieldType = 'AUTOINC')     then Result := ftAutoInc
   else if (FieldType = 'BLOB')        then Result := ftBlob
   else if (FieldType = 'MEMO')        then Result := ftMemo
   else if (FieldType = 'GRAPHIC')     then Result := ftGraphic
   else if (FieldType = 'FMTMEMO')     then Result := ftFmtMemo
   else if (FieldType = 'PARADOXOLE')  then Result := ftParadoxOle
   else if (FieldType = 'DBASEOLE')    then Result := ftDBaseOle
   else if (FieldType = 'TYPEDBINARY') then Result := ftTypedBinary
   else if (FieldType = 'CURSOR')      then Result := ftCursor
   else if (FieldType = 'FIXEDCHAR')   then Result := ftFixedChar
   else if (FieldType = 'WIDESTRING')  then Result := ftWideString
   else if (FieldType = 'LARGEINT')    then Result := ftLargeint
   else if (FieldType = 'ADT')         then Result := ftADT
   else if (FieldType = 'ARRAY')       then Result := ftArray
   else if (FieldType = 'REFERENCE')   then Result := ftReference
   else if (FieldType = 'DATASET')     then Result := ftDataSet
   else if (FieldType = 'ORABLOB')     then Result := ftOraBlob
   else if (FieldType = 'ORACLOB')     then Result := ftOraClob
   else if (FieldType = 'VARIANT')     then Result := ftVariant
   else if (FieldType = 'INTERFACE')   then Result := ftInterface
   else if (FieldType = 'IDISPATCH')   then Result := ftIDispatch
   else if (FieldType = 'GUID')        then Result := ftGuid
   else if (FieldType = 'TIMESTAMP')   then Result := ftTimeStamp
   else if (FieldType = 'FMTBCD')      then Result := ftFMTBcd;
end;

function GetFieldSizeByType(const FieldType : TFieldType; const Size : Integer = 0) : Integer;
begin
    Result := Size;
    case FieldType of
     ftUnknown           : ;
     ftString            : ;
     ftSmallint          : Result := SizeOf(Byte); //todo: check this
     ftInteger           : Result := SizeOf(Integer);
     ftWord              : Result := SizeOf(Word);
     ftBoolean           : Result := SizeOf(Boolean);
     ftFloat             : Result := SizeOf(Double);
     ftCurrency          : Result := SizeOf(Currency);
     ftBCD               : ;
     ftDate, ftDateTime, ftTime, ftTimeStamp  : Result := SizeOf(TDateTime);
     ftBytes             : ;
     ftVarBytes          : ;
     ftAutoInc           : ;
     ftBlob              : ;
     ftMemo              : ;
     ftGraphic           : ;
     ftFmtMemo           : ;
     ftParadoxOle        : ;
     ftDBaseOle          : ;
     ftTypedBinary       : ;
     ftCursor            : ;
     ftFixedChar         : ;
     ftWideString        : ;
     ftLargeint          : ;
     ftADT               : ;
     ftArray             : ;
     ftReference         : ;
     ftDataSet           : ;
     ftOraBlob           : ;
     ftOraClob           : ;
     ftVariant           : ;
     ftInterface         : ;
     ftIDispatch         : ;
     ftGuid              : ;
     ftFMTBcd            : ;
   end;
end;

function GetFieldNodeByName(const AParent : TDOMElement; AFieldName : String) : TDOMElement;
var i : Integer;
begin
// AParent = <row> ... </row>
// ChildNodes.Item[i] = <field ... />
  Result := nil;
  AFieldName := AnsiUpperCase(AFieldName);

  for i := 0 to AParent.ChildNodes.Count - 1 do
    if (AnsiUpperCase(TDOMElement(AParent.ChildNodes.Item[i]).AttribStrings[cField_Name]) = AFieldName) then
      begin
         Result := TDOMElement(AParent.ChildNodes.Item[i]);
         exit;
       end;
end;

(*******************************************************************************
{ TBaseXMLDataSet }
*******************************************************************************)

procedure TBaseXMLDataSet.SetReadOnly(Value: Boolean);
begin
  if (Value <> FReadOnly) then
    begin
     if Active then DatabaseError('Cannot change readonly property when dataset is active');
     FReadOnly:=Value;
    end;
end;

procedure TBaseXMLDataSet.SetXMLDoc(const AValue: TXMLDocument);
begin
  if Active then
     DatabaseError('Cannot change XMLDoc property when dataset is active');
  FXMLDoc := AValue;
end;

function TBaseXMLDataSet.DoOpen: Boolean;
begin
  if not Assigned(FXMLDoc) then
     DatabaseError('XMLDoc is not assigned');
  FCurRec:=-1;
  // initialize internal counters
  FDeletedCount  := 0;
  FInsertedCount := 0;
  FModifiedCount := 0;
  FInternalID    := -1;
  AssignInternalIDS;
  
  Result := true; //todo : true or <rowdata>.childs.count > 0 ?
end;

procedure TBaseXMLDataSet.DoClose;
begin
  FNode := nil;
//  FXMLDoc := nil; //todo : check this
end;

procedure TBaseXMLDataSet.DoDeleteRecord;
begin
// mark as deleted
  AddXMLRecordToSection(
    TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec]),
    cDeletedRecords
    );

// remove record
  DeleteRecordFromXML(
    TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec])
    );
end;

procedure TBaseXMLDataSet.DoCreateFieldDefs;
var i : Integer;
    FieldSize : Word;
    domNode : TDOMElement;
    FieldName : String;
    Required : Boolean;
    ftFieldType : TFieldType;
begin
  if (not Assigned(FXMLDoc)) or
     (csDesigning in ComponentState) then
     exit;

  FieldDefs.Clear;
  for i := 0 to FXMLDoc.DocumentElement.FindNode(cMetaData).FindNode(cFieldDefs).ChildNodes.Count - 1 do
      begin   // Add fields
        domNode := TDOMElement(FXMLDoc.DocumentElement.FindNode(cMetadata).FindNode(cFieldDefs).ChildNodes.Item[i]);
        FieldName := Trim(domNode.AttribStrings[cFieldDef_Name]);
        ftFieldType := GetFieldTypeFromString(Trim(domNode.AttribStrings[cFieldDef_DataType]));
        Required := AnsiLowerCase(domNode.AttribStrings[cFieldDef_Required]) = cTrue;
        // determine field size
        FieldSize := GetFieldSizeByType(ftFieldType,StrToInt(domNode.AttribStrings[cFieldDef_FieldSize]));

        FieldDefs.Add(FieldName, ftFieldType, FieldSize, Required);
        FieldDefs.Items[i].DisplayName := domNode.AttribStrings[cFieldDef_DisplayLabel];
      end;
end;

function TBaseXMLDataSet.GetFieldValue(Field: TField): Variant;
var FieldNode : TDOMElement;
begin
   FieldNode := GetFieldNodeByName(FNode,Field.FieldName);
{$IFDEF USEWIDESTRINGS}
   Result := WideCharToString(@FieldNode.AttribStrings[cField_Value][1]);
{$ELSE}
   Result := FieldNode.AttribStrings[cField_Value];
{$ENDIF}
end;

procedure TBaseXMLDataSet.SetFieldValue(Field: TField; Value: Variant);
var FieldNode : TDOMElement;
begin
   FieldNode := GetFieldNodeByName(FNode,Field.FieldName);

//todo : N.B. Base64 here. do not add 'oldvalue' when record is inserted
   // set old data if we are editing
   if (State = dsEdit) and
      (VarToStr(Value) <> FieldNode.AttribStrings[cField_Value]) and
      (FieldNode.AttribStrings[cField_OldValue] = '') then
     FieldNode.AttribStrings[cField_OldValue] := FieldNode.AttribStrings[cField_Value];
     
// here it should be no problem assigning String to WideString;
   FieldNode.AttribStrings[cField_Value] := Value;
end;

procedure TBaseXMLDataSet.GetBlobField(Field: TField; Stream: TStream);
begin
//todo : implement
end;

procedure TBaseXMLDataSet.SetBlobField(Field: TField; Stream: TStream);
begin
//todo : implement
end;

procedure TBaseXMLDataSet.DoFirst;
begin
  FCurRec := -1;
end;

procedure TBaseXMLDataSet.DoLast;
begin
  FCurRec := RecordCount;
end;

function TBaseXMLDataSet.Navigate(GetMode: TGetMode): TGetResult;
begin
  if (RecordCount < 1)
    then Result := grEOF
    else
      begin
        Result:=grOK;
        case GetMode of
          gmNext:    if (FCurRec >= RecordCount-1)
                       then Result := grEOF
                       else Inc(FCurRec);
          gmPrior:   if (FCurRec <= 0)
                       then begin Result := grBOF; FCurRec := -1; end
                       else Dec(FCurRec);
          gmCurrent: if (FCurRec < 0) or (FCurRec >= RecordCount) then
                        Result := grError;
        end; // case
      end; // else
end;

function TBaseXMLDataSet.AllocateRecordID: Pointer;
begin
  Result := Pointer(FCurRec);
end;

procedure TBaseXMLDataSet.DisposeRecordID(Value: Pointer);
begin
  //Do nothing, no need to dispose since pointer is just an integer
end;

procedure TBaseXMLDataSet.GotoRecordID(Value: Pointer);
begin
  FCurRec := Integer(Value);
end;

function TBaseXMLDataSet.GetBookMarkSize: Integer;
begin
  Result := SizeOf(Integer);
end;

procedure TBaseXMLDataSet.AllocateBookMark(RecordID: Pointer; ABookmark: Pointer);
begin
  PInteger(ABookmark)^:=Integer(RecordID);
end;

procedure TBaseXMLDataSet.DoGotoBookmark(ABookmark: Pointer);
begin
  GotoRecordID(Pointer(PInteger(ABookmark)^));
end;

procedure TBaseXMLDataSet.DoBeforeGetFieldValue;
begin
  FNode := TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec]);
end;

procedure TBaseXMLDataSet.DoAfterGetFieldValue;
begin
  if not ((FNode <> nil) and (State = dsInsert)) then
     FNode := nil;
end;

procedure TBaseXMLDataSet.DoBeforeSetFieldValue(Inserting: Boolean);
var LRecData  : TDOMElement;
    LOldState : Longint;
begin
  try
    LRecData := TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData));

    if Inserting then
       begin // create a <row> element
         FNode := CreateRowWithFields(ROW_INSERTED);
         // add to <recorddata>
         LRecData.AppendChild(FNode);
         // increase record count
         LRecData.AttribStrings[cCount] := IntToStr(RecordCount);
       end
    else
       begin
         FNode := TDOMElement(LRecData.ChildNodes.Item[FCurRec]);
         LOldState := StrToInt(FNode.AttribStrings[cRow_State]);
         FNode.AttribStrings[cRow_State] := IntToStr(LOldState or ROW_MODIFIED);
       end;
  finally
    LRecData := nil; // clear reference
  end;
end;

procedure TBaseXMLDataSet.DoAfterSetFieldValue(Inserting: Boolean);
// todo : fix this. don't know if it's needed
var Index : Integer;
begin
  if Inserting then
     begin
      Index := FindRowIndexInSection(FNode,cRecordData);
      if (Index >= 0) then
         FCurRec := Index;
     end;
  FNode := nil;
end;

function TBaseXMLDataSet.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

function TBaseXMLDataSet.GetRecordCount: Integer;
begin
  Result := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count;
end;

function TBaseXMLDataSet.GetRecNo: Integer;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0)
    then Result := 1
    else Result := FCurRec + 1;
end;

procedure TBaseXMLDataSet.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value < RecordCount) then
    begin
      FCurRec := Value-1;
      Resync([]);
    end;
end;

procedure TBaseXMLDataSet.DeleteRecordFromXML(const ANode: TDOMElement);
// removes record from <recorddata> section
var LRecords : TDOMElement;
begin
  try
    LRecords := TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData));
// remove node from <recorddata> section
    LRecords.RemoveChild(ANode);

// decrease record count
    if FCurRec >= LRecords.ChildNodes.Count then Dec(FCurRec);
    LRecords.AttribStrings[cCount] := IntToStr(RecordCount);
  finally
    LRecords := nil; // clear reference
  end;
end;

function TBaseXMLDataSet.CreateRowWithFields(const AState : Longint): TDOMElement;
// creates a <row> element with corresponding <field> sub-elements
var i : Integer;
    LField : TDOMElement;
begin
  try
    Result := FXMLDoc.CreateElement(cRow); // create <row>
    Result.AttribStrings[cRow_ID] := sGetNextID;
    Result.AttribStrings[cRow_State] := IntToStr(AState);
    
    for i := 0 to FieldDefs.Count - 1 do
       begin
         LField := FXMLDoc.CreateElement(cField); // create <field>
         LField.AttribStrings[cField_Name] := FieldDefs.Items[i].Name;
         LField.AttribStrings[cField_Value] := '';
         case FieldDefs.Items[i].DataType of
           ftInteger : LField.AttribStrings[cField_DataType] := 'integer';
           ftString  : LField.AttribStrings[cField_DataType] := 'string';
         end; // case
         Result.AppendChild(LField); // append <field> to <row>
       end; // for
  finally
    LField := nil; // clear reference
  end;
end;

procedure TBaseXMLDataSet.AddXMLRecordToSection(const ANode: TDOMElement; const ASection : String);
// add record to specified section
// <insertedrecords>, <modifiedrecords>, <deletedrecords>
var LSection : TDOMElement;
    LCount : Longint;
begin
  try
    LSection := TDOMElement(FXMLDoc.DocumentElement.FindNode(ASection));
    if not Assigned(LSection) then
       begin // create section
         LSection := FXMLDoc.CreateElement(ASection);
         LSection.AttribStrings[cCount] := '0';
         FXMLDoc.DocumentElement.AppendChild(LSection);
       end;

// add record to section
    if (ASection = cDeletedRecords)
      then LSection.AppendChild(ANode.CloneNode(true,ANode.OwnerDocument))
      else LSection.AppendChild(ANode);

// increase section records count
    if (ASection = cDeletedRecords)
      then begin inc(FDeletedCount); LCount := FDeletedCount; end;
(*
      else if (ASection = cInsertedRecords)
             then begin inc(FInsertedCount); LCount := FInsertedCount; end
             else if (ASection = cModifiedRecords)
                    then begin inc(FModifiedCount); LCount := FModifiedCount; end;
*)
    LSection.AttribStrings[cCount] := IntToStr(LCount);
  finally
    LSection := nil; // clear reference
  end;
end;

function TBaseXMLDataSet.nGetNextID: Longint;
begin
  inc(FInternalID);
  Result := FInternalID;
end;

function TBaseXMLDataSet.sGetNextID: String;
begin
  Result := IntToStr(nGetNextID);
end;

procedure TBaseXMLDataSet.AssignInternalIDS;
var i : Longint;
    domNode : TDOMElement;
begin
  try
    for i := 0 to FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count - 1 do
      begin
        domNode := TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[i]);
        domNode.AttribStrings[cRow_ID] := sGetNextID;
        domNode.AttribStrings[cRow_State] := IntToStr(ROW_NOT_MODIFIED);
      end;
  finally
    domNode := nil; // clear reference
  end;
end;

function TBaseXMLDataSet.FindRowIndexInSection(const ARow: TDOMElement; const ASection : String): Longint;
var i : Longint;
    LSection : TDOMElement;
begin
  try
    Result := -1;
    LSection := TDOMElement(FXMLDoc.DocumentElement.FindNode(ASection));
    if Assigned(LSection) then
      for i := 0 to LSection.ChildNodes.Count - 1 do
        if (ARow.AttribStrings[cRow_ID] =
            TDOMElement(LSection.ChildNodes.Item[i]).AttribStrings[cRow_ID]) then
           begin
              Result := i;
              exit;
           end;
  finally
    LSection := nil; // clear reference
  end;
end;

constructor TBaseXMLDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLDoc := TXMLDocument.Create;
end;

constructor TBaseXMLDataSet.Create(AOwner: TComponent; AXMLDoc: TXMLDocument);
begin
  Self.Create(AOwner);
  FXMLDoc := AXMLDoc;
end;

constructor TBaseXMLDataSet.Create(AOwner: TComponent; AXML: String);
begin
  Self.Create(AOwner);
  FXMLDoc.DocumentElement.NodeValue := AXML;
end;

destructor TBaseXMLDataSet.Destroy;
begin
  FXMLDoc.Free;
  inherited Destroy;
end;

(*******************************************************************************
{ TBaseSQLConnection }
*******************************************************************************)

procedure TBaseSQLConnection.DoConnect;
begin
  raise Exception.Create('TBaseSQLConnection.DoConnect - not implemented');
end;

procedure TBaseSQLConnection.DoDisconnect;
begin
  raise Exception.Create('TBaseSQLConnection.DoDisconnect - not implemented');
end;

function TBaseSQLConnection.GetConnected: Boolean;
begin
  Result := False;
end;

function TBaseSQLConnection.GetDataSet(Index: Integer): TBaseXMLDataSet;
begin
  Result := TBaseXMLDataSet(FDataSets.Items[Index]^);
end;

function TBaseSQLConnection.GetDataSetCount: Integer;
begin
  Result := FDataSets.Count;
end;

procedure TBaseSQLConnection.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedConnected then SetConnected(True);
  except
    if (csDesigning in ComponentState)
       then if Assigned(Classes.ApplicationHandleException)
               then Classes.ApplicationHandleException(ExceptObject)
                else ShowException(ExceptObject, ExceptAddr)
       else raise;
  end;
end;

procedure TBaseSQLConnection.RegisterClient(Client: TObject; Event: TConnectChangeEvent);
begin
  FClients.Add(Client);
  FConnectEvents.Add(TMethod(Event).Code);
  if Client is TBaseXMLDataSet then
     FDataSets.Add(Client);
end;

procedure TBaseSQLConnection.SetConnected(Value: Boolean);
begin
  if (csReading in ComponentState) and Value
   then FStreamedConnected := True
   else
     begin
       if Value = GetConnected then Exit;
       if Value then
         begin
           if Assigned(BeforeConnect) then BeforeConnect(Self);
           DoConnect;
           SendConnectEvent(True);
           if Assigned(AfterConnect) then AfterConnect(Self);
         end
       else
         begin
           if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
           SendConnectEvent(False);
           DoDisconnect;
           if Assigned(AfterDisconnect) then AfterDisconnect(Self);
         end;
     end;
end;

procedure TBaseSQLConnection.SendConnectEvent(Connecting: Boolean);
var i : Integer;
    ConnectEvent: TConnectChangeEvent;
begin
  for I := 0 to FClients.Count - 1 do
    begin
      if FConnectEvents[I] <> nil then
         begin
           TMethod(ConnectEvent).Code := FConnectEvents[I];
           TMethod(ConnectEvent).Data := FClients[I];
           ConnectEvent(Self, Connecting);
         end;
// todo : fix this deConnectChanged
//(*
      if TObject(FClients.Items[i]^) is TBaseXMLDataSet then
         TBaseXMLDataSet(FClients.Items[i]^).DataEvent(deConnectChange, Ptrint(Connecting));
//*)
    end;
end;

procedure TBaseSQLConnection.UnRegisterClient(Client: TObject);
var Index: Integer;
begin
  if Client is TBaseXMLDataSet then
     FDataSets.Remove(Client);
  Index := FClients.IndexOf(Client);
  if Index <> -1 then
    begin
      FClients.Delete(Index);
      FConnectEvents.Delete(Index);
    end;
end;

constructor TBaseSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSets := TList.Create;
  FClients := TList.Create;
  FConnectEvents := TList.Create;
end;

destructor TBaseSQLConnection.Destroy;
begin
  SetConnected(False);
  FreeAndNil(FConnectEvents);
  FreeAndNil(FClients);
  FreeAndNil(FDataSets);
  inherited Destroy;
end;

procedure TBaseSQLConnection.Open;
begin
  SetConnected(True);
end;

procedure TBaseSQLConnection.Close;
begin
  SetConnected(False);
end;

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
  raise exception.create('not implemented');
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

