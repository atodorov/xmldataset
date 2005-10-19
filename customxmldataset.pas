unit customxmldataset;

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

// a workaround widestrings. We use this because DOMString = WideString
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
// - add display format for Float and DateTime fields. all variantions (Date, Time, Float, Currency)
// - when deleting, editing more than once, inserting, canceling => N.B. internal ID
// - add some events to Dataset / Query
// - change all Integer to Longword / Longint N.B. sign / range
// - add 'oldvalue' = 'value' for primary key fields (to be able to handle SmartXML)

  { TCustomXMLDataSet - dataset that works with XML instead a database }
  TCustomXMLDataSet=class(TGXBaseDataset)
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
    function  GetXMLStringStream: TStringStream;
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
    { helper functions }
    class function GetFieldTypeFromString(FieldType : String) : TFieldType;
    class function GetStringFromFieldType(const FieldType : TFieldType) : String;
    class function GetFieldSizeByType(const FieldType : TFieldType; const Size : Integer = 0) : Integer;
    class function GetFieldNodeByName(const AParent : TDOMElement; AFieldName : String) : TDOMElement;
    class function EncodeBase64(const S : String)  : String; overload;
    class function EncodeBase64(const S : TStream) : String; overload;
    class function DecodeBase64ToString(const S : String) : String;
    class procedure DecodeBase64ToStream(const S : String; Output : TStream);
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(AOwner: TComponent; AXMLDoc : TXMLDocument); virtual; overload;
    constructor Create(AOwner: TComponent; AXML : String); virtual; overload;
    destructor  Destroy; override;
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property XMLDocument : TXMLDocument read FXMLDoc write SetXMLDoc;
    property XMLStringStream : TStringStream read GetXMLStringStream;
  end;


  
implementation

uses uXMLDSConsts, Variants, Base64;

{$IFDEF DEBUGXML}
procedure Log(const Msg : String);
begin
  Memo.Lines.Add(DateTimeToStr(Now)+' : '+Msg);
end;
{$ENDIF}


(*******************************************************************************
{ TCustomXMLDataSet }
*******************************************************************************)

class function TCustomXMLDataSet.EncodeBase64(const S : String) : String;
var S1, S2 : TStringStream;
begin
  S1 := TStringStream.Create(S);
  try
    S1.Position:=0;
    S2 := TStringStream.Create('');
    try
      with TBase64EncodingStream.Create(S2) do
        try
          CopyFrom(S1, S1.Size);
        finally
          Free;
        end;
      Result := S2.DataString;
    finally
      S2.Free;
    end;
 finally
   S1.Free;
 end;
end;

class function TCustomXMLDataSet.EncodeBase64(const S : TStream) : String;
var strStrm : TStringStream;
begin
  S.Position := 0;
  strStrm := TStringStream.Create('');
  try
    with TBase64EncodingStream.Create(strStrm) do
      try
        CopyFrom(S, S.Size);
      finally
        Free;
      end;
    Result := strStrm.DataString;
  finally
    strStrm.Free;
  end;
end;

class function TCustomXMLDataSet.DecodeBase64ToString(const S : String) : String;
var S1, S2 : TStringStream;
    b64Decoder : TBase64DecodingStream;
begin
  S1 := TStringStream.Create(S);
  try
    S1.Position:=0;
    S2 := TStringStream.Create('');
    try
      b64Decoder := TBase64DecodingStream.Create(S1);
      S2.CopyFrom(b64Decoder, b64Decoder.Size);
    finally
      b64Decoder.Free;
    end;
    Result := S2.DataString;
 finally
   S2.Free;
   S1.Free;
 end;
end;

class procedure TCustomXMLDataSet.DecodeBase64ToStream(const S : String; Output : TStream);
var Strm : TStringStream;
    b64Decoder : TBase64DecodingStream;
begin
  try
     Strm := TStringStream.Create(S);
     Strm.Position := 0;
  
     b64Decoder := TBase64DecodingStream.Create(Strm);
     Output.CopyFrom(b64Decoder, b64Decoder.Size);
  finally
    b64Decoder.Free;
    Strm.Free;
  end;
end;

class function TCustomXMLDataSet.GetFieldTypeFromString(FieldType : String) : TFieldType;
//todo : changed this using constant array in new DB.pp
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

class function TCustomXMLDataSet.GetStringFromFieldType(const FieldType : TFieldType) : String;
//todo : changed this using constant array in new DB.pp
begin
   case FieldType of
     ftUnknown     : Result := 'UNKNOWN';
     ftString      : Result := 'STRING';
     ftSmallint    : Result := 'SMALLINT';
     ftInteger     : Result := 'INTEGER';
     ftWord        : Result := 'WORD';
     ftBoolean     : Result := 'BOOLEAN';
     ftFloat       : Result := 'FLOAT';
     ftCurrency    : Result := 'CURRENCY';
     ftBCD         : Result := 'BCD';
     ftDate        : Result := 'DATE';
     ftTime        : Result := 'TIME';
     ftDateTime    : Result := 'DATETIME';
     ftBytes       : Result := 'BYTES';
     ftVarBytes    : Result := 'VARBYTES';
     ftAutoInc     : Result := 'AUTOINC';
     ftBlob        : Result := 'BLOB';
     ftMemo        : Result := 'MEMO';
     ftGraphic     : Result := 'GRAPHIC';
     ftFmtMemo     : Result := 'FMTMEMO';
     ftParadoxOle  : Result := 'PARADOXOLE';
     ftDBaseOle    : Result := 'DBASEOLE';
     ftTypedBinary : Result := 'TYPEDBINARY';
     ftCursor      : Result := 'CURSOR';
     ftFixedChar   : Result := 'FIXEDCHAR';
     ftWideString  : Result := 'WIDESTRING';
     ftLargeint    : Result := 'LARGEINT';
     ftADT         : Result := 'ADT';
     ftArray       : Result := 'ARRAY';
     ftReference   : Result := 'REFERENCE';
     ftDataSet     : Result := 'DATASET';
     ftOraBlob     : Result := 'ORABLOB';
     ftOraClob     : Result := 'ORACLOB';
     ftVariant     : Result := 'VARIANT';
     ftInterface   : Result := 'INTERFACE';
     ftIDispatch   : Result := 'IDISPATCH';
     ftGuid        : Result := 'GUID';
     ftTimeStamp   : Result := 'TIMESTAMP';
     ftFMTBcd      : Result := 'FMTBCD';
     else            Result := 'UNKNOWN';
   end;
   Result := AnsiLowerCase(Result);
end;

class function TCustomXMLDataSet.GetFieldSizeByType(const FieldType : TFieldType; const Size : Integer = 0) : Integer;
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

class function TCustomXMLDataSet.GetFieldNodeByName(const AParent : TDOMElement; AFieldName : String) : TDOMElement;
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

procedure TCustomXMLDataSet.SetReadOnly(Value: Boolean);
begin
  if (Value <> FReadOnly) then
    begin
     if Active then DatabaseError('Cannot change readonly property when dataset is active');
     FReadOnly:=Value;
    end;
end;

function TCustomXMLDataSet.GetXMLStringStream: TStringStream;
begin //todo : possible memory leaks
  Result := TStringStream.Create(XMLDocument.NodeValue);
end;

procedure TCustomXMLDataSet.SetXMLDoc(const AValue: TXMLDocument);
begin
  if Active then
     DatabaseError('Cannot change XMLDoc property when dataset is active');
  FXMLDoc := AValue;
end;

function TCustomXMLDataSet.DoOpen: Boolean;
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

procedure TCustomXMLDataSet.DoClose;
begin
  FNode := nil;
//  FXMLDoc := nil; //todo : check this
end;

procedure TCustomXMLDataSet.DoDeleteRecord;
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

procedure TCustomXMLDataSet.DoCreateFieldDefs;
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
  if (XMLDocument.DocumentElement.AttribStrings[cDocument_Type] <> cDocument_Type_Datapacket) then
     raise Exception.Create('Invalid XML Document!');
     
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

function TCustomXMLDataSet.GetFieldValue(Field: TField): Variant;
var FieldNode : TDOMElement;
    strValue : String;
begin
   FieldNode := GetFieldNodeByName(FNode,Field.FieldName);
{$IFDEF USEWIDESTRINGS}
   strValue := WideCharToString(@FieldNode.AttribStrings[cField_Value][1]);
{$ELSE}
   strValue := FieldNode.AttribStrings[cField_Value];
{$ENDIF}
   Result := DecodeBase64ToString(strValue);
end;

procedure TCustomXMLDataSet.SetFieldValue(Field: TField; Value: Variant);
var FieldNode : TDOMElement;
    strEnc64 : String;
begin
   FieldNode := GetFieldNodeByName(FNode,Field.FieldName);
   strEnc64 := EncodeBase64(Value);
   // set old data if we are editing
   if (State = dsEdit) and
      ((StrToInt(FNode.AttribStrings[cRow_State]) and ROW_INSERTED) <> ROW_INSERTED) and
      (strEnc64 <> FieldNode.AttribStrings[cField_Value]) and
      (FieldNode.AttribStrings[cField_OldValue] = '') then
     FieldNode.AttribStrings[cField_OldValue] := FieldNode.AttribStrings[cField_Value];

   FieldNode.AttribStrings[cField_Value] := strEnc64;
end;

procedure TCustomXMLDataSet.GetBlobField(Field: TField; Stream: TStream);
var LBlob : TDOMElement;
    strTemp : String;
begin
  if not Assigned(FNode) then exit;
  try
    LBlob := GetFieldNodeByName(FNode, Field.FieldName);
    if not Assigned(LBlob)
      then strTemp := ''
      else strTemp := LBlob.AttribStrings[cField_Value];
    DecodeBase64ToStream(strTemp,Stream);
  finally
    LBlob := nil; // clear reference
  end;
end;

procedure TCustomXMLDataSet.SetBlobField(Field: TField; Stream: TStream);
var LBlob : TDOMElement;
    strEnc64 : String;
begin
  if not Assigned(FNode) then exit;
  try
    LBlob := GetFieldNodeByName(FNode, Field.FieldName);
    strEnc64 := EncodeBase64(Stream);
    
    // set old data if we are editing
    if (State = dsEdit) and
       ((StrToInt(FNode.AttribStrings[cRow_State]) and ROW_INSERTED) <> ROW_INSERTED) and
       (strEnc64 <> LBlob.AttribStrings[cField_Value]) and
       (LBlob.AttribStrings[cField_OldValue] = '') then
     LBlob.AttribStrings[cField_OldValue] := LBlob.AttribStrings[cField_Value];
     
    LBlob.AttribStrings[cField_Value] := strEnc64;
  finally
    LBlob := nil; // clear reference
  end;
end;

procedure TCustomXMLDataSet.DoFirst;
begin
  FCurRec := -1;
end;

procedure TCustomXMLDataSet.DoLast;
begin
  FCurRec := RecordCount;
end;

function TCustomXMLDataSet.Navigate(GetMode: TGetMode): TGetResult;
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

function TCustomXMLDataSet.AllocateRecordID: Pointer;
begin
  Result := Pointer(FCurRec);
end;

procedure TCustomXMLDataSet.DisposeRecordID(Value: Pointer);
begin
  //Do nothing, no need to dispose since pointer is just an integer
end;

procedure TCustomXMLDataSet.GotoRecordID(Value: Pointer);
begin
  FCurRec := Integer(Value);
end;

function TCustomXMLDataSet.GetBookMarkSize: Integer;
begin
  Result := SizeOf(Integer);
end;

procedure TCustomXMLDataSet.AllocateBookMark(RecordID: Pointer; ABookmark: Pointer);
begin
  PInteger(ABookmark)^:=Integer(RecordID);
end;

procedure TCustomXMLDataSet.DoGotoBookmark(ABookmark: Pointer);
begin
  GotoRecordID(Pointer(PInteger(ABookmark)^));
end;

procedure TCustomXMLDataSet.DoBeforeGetFieldValue;
begin
  FNode := TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec]);
end;

procedure TCustomXMLDataSet.DoAfterGetFieldValue;
begin
  if not ((FNode <> nil) and (State = dsInsert)) then
     FNode := nil;
end;

procedure TCustomXMLDataSet.DoBeforeSetFieldValue(Inserting: Boolean);
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

procedure TCustomXMLDataSet.DoAfterSetFieldValue(Inserting: Boolean);
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

function TCustomXMLDataSet.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

function TCustomXMLDataSet.GetRecordCount: Integer;
begin
  Result := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count;
end;

function TCustomXMLDataSet.GetRecNo: Integer;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0)
    then Result := 1
    else Result := FCurRec + 1;
end;

procedure TCustomXMLDataSet.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value < RecordCount) then
    begin
      FCurRec := Value-1;
      Resync([]);
    end;
end;

procedure TCustomXMLDataSet.DeleteRecordFromXML(const ANode: TDOMElement);
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

function TCustomXMLDataSet.CreateRowWithFields(const AState : Longint): TDOMElement;
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
         LField.AttribStrings[cField_DataType] := GetStringFromFieldType(FieldDefs.Items[i].DataType);
         Result.AppendChild(LField); // append <field> to <row>
       end; // for
  finally
    LField := nil; // clear reference
  end;
end;

procedure TCustomXMLDataSet.AddXMLRecordToSection(const ANode: TDOMElement; const ASection : String);
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

function TCustomXMLDataSet.nGetNextID: Longint;
begin
  inc(FInternalID);
  Result := FInternalID;
end;

function TCustomXMLDataSet.sGetNextID: String;
begin
  Result := IntToStr(nGetNextID);
end;

procedure TCustomXMLDataSet.AssignInternalIDS;
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

function TCustomXMLDataSet.FindRowIndexInSection(const ARow: TDOMElement; const ASection : String): Longint;
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

constructor TCustomXMLDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLDoc := TXMLDocument.Create;
end;

constructor TCustomXMLDataSet.Create(AOwner: TComponent; AXMLDoc: TXMLDocument);
begin
  Self.Create(AOwner);
  FXMLDoc := AXMLDoc;
end;

constructor TCustomXMLDataSet.Create(AOwner: TComponent; AXML: String);
begin
  Self.Create(AOwner);
  FXMLDoc.DocumentElement.NodeValue := AXML;
end;

destructor TCustomXMLDataSet.Destroy;
begin
  FXMLDoc.Free;
  inherited Destroy;
end;

end.

