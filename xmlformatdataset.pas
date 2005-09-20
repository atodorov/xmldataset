unit XMLFormatDataSet;

{$mode objfpc}{$H+}

{$DEFINE DEBUGXML}

{*******************************************************************************
 TXMLFormatDataSet.
 (c) 2005 - Alexander Todorov,
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
  Classes, SysUtils, Variants, DB, DOM;

const

// XML attribute values constants
  cYes   = 'yes';
  cNo    = 'no';
  cTrue  = 'true';
  cFalse = 'false';
  
// XML node and attribute name constants

// <datapacket version="2.000" destination="" day="28" month="8" year="2005" hour="20" min="57" sec="12" msec="46">
  cDatapacket            = 'datapacket';
  cDatapacketVersion     = 'version';
  cDatapacketDestination = 'destination';
  cDatapacketDay         = 'day';
  cDatapacketMonth       = 'month';
  cDatapacketYear        = 'year';
  cDatapacketHour        = 'hour';
  cDatapacketMin         = 'min';
  cDatapacketSec         = 'sec';
  cDatapacketMSec        = 'msec';
  
// <producer name="alexx" url="" description="written by hand" />
  cProducer            = 'producer';
  cProducerName        = 'name';
  cProducerURL         = 'url';
  cProducerDescription = 'description';
  
// <metadata fielddefs="yes" indexdefs="no" recorddata="yes" changes="yes">
  cMetadata           = 'metadata';
  cMetadataFieldDefs  = 'fielddefs';
  cMetadataIndexDefs  = 'indexdefs';
  cMetadataRecordData = 'recorddata';
  cMetadataChanges    = 'changes';

// <fielddefs count="2">
  cFieldDefs      = 'fielddefs';
  cFieldDefsCount = 'count';
  
// <fielddef name="ID" fieldkind="data" datatype="integer" fieldsize="0" displaylabel="ID" displaywidth="10" fieldindex="0" required="true" readonly="false"/>
  cFieldDef             = 'fielddef';
  cFieldDefName         = 'name';
  cFieldDefFieldKind    = 'fieldkind';
  cFieldDefDataType     = 'datatype';
  cFieldDefFieldSize    = 'fieldsize';
  cFieldDefDisplayLabel = 'displaylabel';
  cFieldDefDisplayWidth = 'displaywidth';
  cFieldDefFieldIndex   = 'fieldindex';
  cFieldDefRequired     = 'required';
  cFieldDefReadOnly     = 'readonly';

// <indexdefs count="0" />
  cIndexDefs      = 'indexdefs';
  cIndexDefsCount = 'count';
  
// <recorddata count="4" />
  cRecordData      = 'recorddata';
  cRecordDataCount = 'count';
  
// <row>
  cRow = 'row';
  
// <field name="ID" value="0" datatype="integer" />
  cField         = 'field';
  cFieldName     = 'name';
  cFieldValue    = 'value';
  cFieldDataType = 'datatype';
  cFieldSize     = 'size';

type

  { PXMLBuffer }
  PXMLBuffer = ^TXMLBuffer;
  
  { TXMLBuffer }

  TXMLBuffer = class (TObject)
  private
    FXMLNode : TDOMNode; // holds the xml value for a single record
    FBookmarkFlag : TBookmarkFlag;
    FBookmark : Integer;
    FFields : TFields;
    FRecordNumber : PtrInt; // copied from TRecInfo
  protected
    function  CreateFieldFromXML(const AFieldNode : TDOMNode) : TField; virtual; // creates a TField from xml
    function  GetFieldByIndex(i : Integer) : TField;
    procedure CreateFields; // creates FFields from FXMLNode
    procedure SetXMLNode(const ANode : TDOMNode); // sets FXMLNode and re-create fields
  public
    constructor Create; virtual; overload;
    constructor Create(ADOMNode : TDOMNode); virtual; overload;
    destructor  Destroy; override;
    function FieldByName(const AFieldName : String) :TField;
    property BookmarkFlag : TBookmarkFlag read FBookmarkFlag write FBookmarkFlag;
    property Fields[i : Integer] : TField read GetFieldByIndex;
    property RecordNumber : PtrInt read FRecordNumber write FRecordNumber;
    property XMLNode : TDOMNode read FXMLNode write SetXMLNode;
    property BookMark : Integer read FBookmark write FBookmark;
  end;

  { TXMLFormatDataSet }
  TXMLFormatDataSet = class(TDataSet)
  private
    FFilterBuffer : PChar;
    FReadOnly     : Boolean;
    FTrimSpace    : Boolean;
    FRecordCount  : Longint;
    procedure SetXML(const Value: String);
    function  GetXML : String;
    procedure SetXMLDoc(const AValue : TXMLDocument);
    procedure SetTrimSpace(Value : Boolean);
    procedure SetReadOnly(Value : Boolean);
    function  GetActiveRecBuf(var RecBuf: PChar): Boolean;
  protected
    FXMLDoc             :TXMLDocument;
    FCurRec             :Integer;
    FRecordSize         :Integer;
    FLastBookmark       :PtrInt;
    function  AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalEdit; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function  IsCursorOpen: Boolean; override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function  GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function  GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize: Word; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure ClearCalcFields(Buffer: PChar); override;
    function  GetRecordCount: Integer; override;
    function  GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function  GetCanModify: boolean; override;
    function  GetRecordFromXML(Buffer : PChar; GetMode: TGetMode): TGetResult;
    function  RecordFilter(RecBuf: Pointer; ARecNo: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property CanModify;
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property XML : String read GetXML write SetXML;
    property XMLDocument : TXMLDocument read FXMLDoc write SetXMLDoc;
    property TrimSpace: Boolean read FTrimSpace write SetTrimSpace default True;
    property FieldDefs;
    property Active;
    property AutoCalcFields;
    property Filtered;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
//    property BeforeRefresh;
//    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;
  
  { helper functions }
  function GetFieldTypeFromString(FieldType : String) : TFieldType;
  function GetFieldSizeByType(const FieldType : TFieldType; const Size : Integer = 0) : Integer;
  function GetFieldSizeFromXML(FieldNode : TDOMNode; const Size : Integer = 0) : Integer;

implementation

{$IFDEF DEBUGXML}
uses Dialogs;
{$ENDIF}

{ helper functions }
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
     ftString            : ; //todo: Size = MaxFieldLength. pass this from InternalInitFieldDefs
     ftSmallint          : Result := SizeOf(Byte); //todo: check this
     ftInteger           : Result := SizeOf(Integer);
     ftWord              : Result := SizeOf(Word);
     ftBoolean           : Result := SizeOf(Boolean);
     ftFloat             : Result := SizeOf(Double); //todo: Float, Double, Real ???
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

function GetFieldSizeFromXML(FieldNode : TDOMNode; const Size : Integer = 0) : Integer;
begin
   Result := Size;
   if (FieldNode <> nil) and (FieldNode.Attributes <> nil) and
      (FieldNode.Attributes.GetNamedItem(cFieldSize) <> nil) then
      Result := StrToInt(FieldNode.Attributes.GetNamedItem(cFieldSize).NodeValue);
end;


{ TXMLFormatDataSet }
constructor TXMLFormatDataSet.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FRecordSize   := SizeOf(TXMLBuffer); // - SizeOf(TBookmarkFlag);
  FTrimSpace    := TRUE;
  FXMLDoc := TXMLDocument.Create;
end;

destructor TXMLFormatDataSet.Destroy;
begin
  FXMLDoc.Free;
  inherited Destroy;
end;

// sets XML data
procedure TXMLFormatDataSet.SetXML(const Value: String);
begin
  CheckInactive;
  FXMLDoc.NodeValue := Value;
end;

function TXMLFormatDataSet.GetXML: String;
begin
  Result := FXMLDoc.NodeValue;
end;

procedure TXMLFormatDataSet.SetXMLDoc(const AValue: TXMLDocument);
begin
  CheckInactive;
  FXMLDoc := AValue;
end;

procedure TXMLFormatDataSet.SetTrimSpace(Value : Boolean);
begin
  CheckInactive;
  FTrimSpace := Value;
end;

procedure TXMLFormatDataSet.SetReadOnly(Value : Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

// create field definitions based on current scheme
procedure TXMLFormatDataSet.InternalInitFieldDefs;
var i, FieldSize :Integer;
    domNode : TDomNode;
    FieldName : String;
    Required : Boolean;
    ftFieldType : TFieldType;
begin
  if (not Assigned(FXMLDoc)) or
     (csDesigning in ComponentState) then
     exit;

(*
  for i := 0 to FXML.Count - 1 do // Fabricate Bookmarks N.B.
      FXML.Objects[i] := TObject(Pointer(i+1));
*)
  FieldDefs.Clear;
  for i := 0 to FXMLDoc.DocumentElement.FindNode(cMetaData).FindNode(cFieldDefs).ChildNodes.Count - 1 do
      begin   // Add fields
        domNode := FXMLDoc.DocumentElement.FindNode(cMetadata).FindNode(cFieldDefs).ChildNodes.Item[i];
        FieldName := Trim(domNode.Attributes.GetNamedItem(cFieldDefName).NodeValue);
        ftFieldType := GetFieldTypeFromString(Trim(domNode.Attributes.GetNamedItem(cFieldDefDataType).NodeValue));
        Required := AnsiUpperCase(domNode.Attributes.GetNamedItem(cFieldDefRequired).NodeValue) = 'TRUE';
        // determine field size
        FieldSize := GetFieldSizeByType(ftFieldType,StrToInt(domNode.Attributes.GetNamedItem(cFieldDefFieldSize).NodeValue));

        FieldDefs.Add(FieldName, ftFieldType, FieldSize, Required);
      end;
end;

procedure TXMLFormatDataSet.InternalOpen;
begin
  if (csDesigning in ComponentState) then exit;
  FCurRec := -1;
  if not Assigned(FXMLDoc) then
     raise Exception.Create('XML not assigned');

  InternalInitFieldDefs;
  if DefaultFields then
     CreateFields;
  BindFields(TRUE);
  FRecordCount := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count;
//todo: kakvi sa ni bookmarks
  BookmarkSize := SizeOf(Integer);
(* // mai ne ni trqbva ve4e
  FRecInfoOfs := FRecordSize + CalcFieldsSize; // Initialize the offset for TRecInfo in the buffer
  FBookmarkOfs := FRecInfoOfs + SizeOf(TRecInfo);
  FRecBufSize := FBookmarkOfs + BookmarkSize;
*)
//todo: kato izmislime kade sa bookmarks tova 6te se promeni mai
  FLastBookmark := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count;
  // or  FXMLDoc.DocumentElement.FindNode(cRecordData).Attributes.GetNamedItem(cRecordDataCount).NodeValue;
end;

procedure TXMLFormatDataSet.InternalClose;
begin
  BindFields(FALSE);
  if DefaultFields then // Destroy the TField
    DestroyFields;
  FCurRec := -1;        // Reset these internal flags
  FLastBookmark := 0;
  FRecordSize := 0;
end;

function TXMLFormatDataSet.IsCursorOpen: Boolean;
begin
  Result := Assigned(FXMLDoc) and (FRecordSize > 0);
end;

procedure TXMLFormatDataSet.InternalHandleException;
begin
{$ifndef fpc}
   Application.HandleException(Self);
{$endif}
end;

// Record Functions
function TXMLFormatDataSet.AllocRecordBuffer: PChar;
begin // N.B
  Result := Pointer(TXMLBuffer.Create);
end;

procedure TXMLFormatDataSet.FreeRecordBuffer(var Buffer: PChar);
begin // N.B
  TXMLBuffer(Pointer(Buffer)^).Free;
end;

procedure TXMLFormatDataSet.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer[0], FRecordSize, 0);
end;

procedure TXMLFormatDataSet.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[RecordSize], CalcFieldsSize, 0);
end;

function TXMLFormatDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  if (FXMLDoc = nil) or (FXMLDoc.DocumentElement = nil) or
     (FXMLDoc.DocumentElement.FindNode(cRecordData) = nil) or
     (FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes = nil) or
     (FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count < 1)
    then Result := grEOF
    else Result := GetRecordFromXML(Buffer, GetMode);
    

  if (Result = grOK) then
    begin
      if (CalcFieldsSize > 0) then
         GetCalcFields(Buffer);
      with TXMLBuffer(Pointer(Buffer)^) do
        begin
          BookmarkFlag := bfCurrent;
          RecordNumber := FCurRec;
//          XMLNode := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec];
        end;
    end
  else
    if (Result = grError) and DoCheck then
       DatabaseError('No Records');
end;

function TXMLFormatDataSet.GetRecordCount: Longint;
begin
  Result := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count;
end;

function TXMLFormatDataSet.GetRecNo: Longint;
var
  BufPtr: PChar;
begin
  Result := -1;
  if GetActiveRecBuf(BufPtr) then
     Result := TXMLBuffer(Pointer(BufPtr)^).RecordNumber;
end;

procedure TXMLFormatDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value >= 0) and
     (Value < FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count) and
     (Value <> RecNo) then
  begin
    DoBeforeScroll;
    FCurRec := Value - 1;
    Resync([]);
    DoAfterScroll;
  end;
end;

function TXMLFormatDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TXMLFormatDataSet.GetActiveRecBuf(var RecBuf: PChar): Boolean;
begin
  case State of
    dsBrowse: if IsEmpty
                 then RecBuf := nil
                 else RecBuf := ActiveBuffer;
    dsEdit, dsInsert: RecBuf := ActiveBuffer;
    dsCalcFields: RecBuf := CalcBuffer;
    dsFilter: RecBuf := FFilterBuffer;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

function TXMLFormatDataSet.GetRecordFromXML(Buffer : PChar; GetMode: TGetMode): TGetResult;
// return TXMLBuffer object into Buffer
var Accepted : Boolean;
begin
  Result := grOK;
  repeat
    Accepted := TRUE;
    case GetMode of
      gmNext:    if (FCurRec >= RecordCount - 1)
                   then Result := grEOF
                   else inc(FCurRec);
      gmPrior:   if (FCurRec <= 0)
                   then Result := grBOF
                   else dec(FCurRec);
      gmCurrent: if (FCurRec < 0) or (FCurRec >= RecordCount) then
                    Result := grError;
    end;
    if (Result = grOk) then
    begin
//todo: N.B.
        Move(
           Pointer(TXMLBuffer.Create(FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec]))^,
           Buffer^,SizeOf(TXMLBuffer)
           );
      if Filtered then
      begin
        Accepted := RecordFilter(Buffer, FCurRec +1);
        if not Accepted and (GetMode = gmCurrent) then
           inc(FCurRec);
      end;
    end;
  until Accepted;
end;

function TXMLFormatDataSet.RecordFilter(RecBuf: Pointer; ARecNo: Integer): Boolean;
var
  Accept: Boolean;
  SaveState: TDataSetState;
begin                          // Returns true if accepted in the filter
  SaveState := SetTempState(dsFilter);
  FFilterBuffer := RecBuf;
  TXMLBuffer(RecBuf^).RecordNumber := ARecNo;
  Accept := TRUE;
  if Accept and Assigned(OnFilterRecord) then
    OnFilterRecord(Self, Accept);
  RestoreState(SaveState);
  Result := Accept;
end;

function TXMLFormatDataSet.GetCanModify: boolean;
begin
  Result := not FReadOnly;
end;

// Field Related
function TXMLFormatDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var RecBuf: PChar;
    BuffField : TField;
    intValue : Integer;
    dblValue : Double;
    boolValue : Boolean;
    dtValue : TDateTime;
    liValue : Longint;
    strValue : String;
begin
  Result := GetActiveRecBuf(RecBuf);

  if Result and (RecBuf <> nil) then
     begin
       BuffField := TXMLBuffer(Pointer(RecBuf)^).FieldByName(Field.FieldName);
       case BuffField.DataType of
         ftUnknown     : ;
         ftString      :
            begin
              if FTrimSpace
                 then strValue := Trim(BuffField.AsString)
                 else strValue := BuffField.AsString;
              Move(strValue[1],Buffer^,Length(strValue));
            end;
         ftSmallint    : ; //todo: check this
         ftInteger     : begin intValue := BuffField.AsInteger;  Move(intValue,Buffer^,SizeOf(Integer));  end;
         ftWord        : ;
         ftBoolean     : begin boolValue := BuffField.AsBoolean; Move(boolValue,Buffer^,SizeOf(Boolean)); end;
         ftFloat       : begin dblValue := BuffField.AsFloat;    Move(dblValue,Buffer^,SizeOf(Double));   end;
         ftCurrency    : ;
         ftBCD         : ;
         ftDate, ftTime, ftDateTime, ftTimeStamp : begin dtValue := BuffField.AsDateTime; Move(dtValue,Buffer^,SizeOf(TDateTime)); end;
         ftBytes       : ;
         ftVarBytes    : ;
         ftAutoInc     : ;
         ftBlob        : ;
         ftMemo        : ;
         ftGraphic     : ;
         ftFmtMemo     : ;
         ftParadoxOle  : ;
         ftDBaseOle    : ;
         ftTypedBinary : ;
         ftCursor      : ;
         ftFixedChar   : ;
         ftWideString  : ;
         ftLargeint    : begin liValue := BuffField.AsLongint; Move(liValue,Buffer^,SizeOf(Longint)); end;
         ftADT         : ;
         ftArray       : ;
         ftReference   : ;
         ftDataSet     : ;
         ftOraBlob     : ;
         ftOraClob     : ;
         ftVariant     : ;
         ftInterface   : ;
         ftIDispatch   : ;
         ftGuid        : ;
         ftFMTBcd      : ;
//         else Move(Buffer,Pointer(TXMLBuffer(Pointer(RecBuf)^).FieldByName(Field.FieldName).Value)^,Field.Size);
       end;
     end;
end;

procedure TXMLFormatDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf, BufEnd: PChar;
  p : Integer;
begin
  if not (State in [dsEdit, dsInsert]) then
     DatabaseError('Dataset not in edit or insert mode', Self);
  GetActiveRecBuf(RecBuf);
  //todo: moze bi sledva ne6to kato GetFieldData
(*
  if Field.FieldNo > 0 then
  begin
    if State = dsCalcFields then
       DatabaseError('Dataset not in edit or insert mode', Self);
    if Field.ReadOnly and not (State in [dsSetKey, dsFilter]) then
       DatabaseErrorFmt('Field ''%s'' cannot be modified', [Field.DisplayName]);
    Field.Validate(Buffer);

    if Field.FieldKind <> fkInternalCalc then
    begin
      SetFieldPos(RecBuf, Field.FieldNo);
      BufEnd := StrEnd(ActiveBuffer);  // Fill with blanks when necessary
      if BufEnd > RecBuf then
        BufEnd := RecBuf;
      FillChar(BufEnd[0], Field.Size + PtrInt(RecBuf) - PtrInt(BufEnd), Ord(' '));
      p := StrLen(Buffer);
      if p > Field.Size then
        p := Field.Size;
      Move(Buffer^, RecBuf[0], p);
      ActiveBuffer[RecordSize-1] := #0;
    end;
  end
  else // fkCalculated, fkLookup
  begin
    Inc(RecBuf, FRecordSize + Field.Offset);
    Move(Buffer^, RecBuf[0], Field.Size);
  end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Ptrint(Field));
*)
end;

// Navigation / Editing
procedure TXMLFormatDataSet.InternalFirst;
begin
  FCurRec := -1;
end;

procedure TXMLFormatDataSet.InternalLast;
begin
  FCurRec := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count-1;
end;

procedure TXMLFormatDataSet.InternalPost;
begin
  inherited UpdateRecord;
  if (State = dsEdit) // just update the data in the xml document
    then FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec].NodeValue := TXMLBuffer(Pointer(ActiveBuffer)^).XMLNode.NodeValue
    else InternalAddRecord(ActiveBuffer, FALSE);
end;

procedure TXMLFormatDataSet.InternalEdit;
begin
  raise Exception.Create('TXMLFormatDataSet.InternalEdit');
end;

procedure TXMLFormatDataSet.InternalDelete;
begin
//todo: do not delete from XML. mark record as deleted and send to driver
  FXMLDoc.DocumentElement.FindNode(cRecordData).RemoveChild
      (FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec]);
      
  if FCurRec >= FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count then
     Dec(FCurRec);
end;

procedure TXMLFormatDataSet.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);
var RefChild, NewChild :TDOMNode;
begin
  try
    Inc(FLastBookmark);
(*
    if DoAppend then
       InternalLast;
*)
    NewChild := TDOMNode.Create(FXMLDoc);
    NewChild.NodeValue := TXMLBuffer(Pointer(Buffer)^).XMLNode.NodeValue;

    RefChild := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FLastBookmark];

    if DoAppend
      then FXMLDoc.DocumentElement.FindNode(cRecordData).AppendChild(NewChild)
      else FXMLDoc.DocumentElement.FindNode(cRecordData).InsertBefore(NewChild, RefChild);
  finally
    NewChild.Free; // ??????
    RefChild := nil; // clear reference
  end;
end;

procedure TXMLFormatDataSet.InternalGotoBookmark(ABookmark: Pointer);
var Index: Integer;
begin
  Index := Integer(ABookmark^);
  if (Index <> -1)
    then FCurRec := Index
    else DatabaseError('Bookmark not found');
end;

procedure TXMLFormatDataSet.InternalSetToRecord(Buffer: PChar);
begin
  if (State <> dsInsert) then
     InternalGotoBookmark(@TXMLBuffer(Pointer(Buffer)^).BookMark);
end;

function TXMLFormatDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := TXMLBuffer(Pointer(Buffer)^).BookmarkFlag;
end;

procedure TXMLFormatDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  TXMLBuffer(Pointer(Buffer)^).BookmarkFlag := Value;
end;

procedure TXMLFormatDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin // BookmarkSize = ??
  Move(TXMLBuffer(Pointer(Buffer)^).Bookmark, Data, BookmarkSize);
end;

procedure TXMLFormatDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin // BookmarkSize = ??
  Move(Data, TXMLBuffer(Pointer(Buffer)^).Bookmark, BookmarkSize);
end;

{ TXMLBuffer }

function TXMLBuffer.CreateFieldFromXML(const AFieldNode: TDOMNode): TField;
//todo: should Owner of fields be property DataSet : TDataSet
var FieldType : TFieldType;
    Value : String; // Variant;
begin
  Result :=nil;
  if (AFieldNode <> nil) then
     begin
       if AFieldNode.Attributes.GetNamedItem(cFieldDataType) <> nil then
            FieldType := GetFieldTypeFromString(AFieldNode.Attributes.GetNamedItem(cFieldDataType).NodeValue)
       else FieldType := GetFieldTypeFromString('');

{$IFDEF DEBUGXML}
ShowMessage('NodeName = '+AFieldNode.Attributes.GetNamedItem(cFieldName).NodeValue);
ShowMessage('NodeValue = '+AFieldNode.NodeValue);
{$ENDIF}
       if (AFieldNode.Attributes.GetNamedItem(cFieldValue) <> nil) then // simple field
            Value := AFieldNode.Attributes.GetNamedItem(cFieldValue).NodeValue
       else Value := TDOMCharacterData(AFieldNode).Data; // todo: N.B. type casting CDATA = Blob, String, etc

Result:= TField.Create(nil);
Result.AsString := Value;
exit;
(*
       // create field according to type
       case FieldType of //todo : synchronize with defs in fields.inc
         ftUnknown     : begin Result := TField.Create(nil);        Result.Value := Value; end;
         ftString      : begin Result := TStringField.Create(nil);  Result.AsString := VarToStr(Value); end;
         ftSmallint, ftInteger, ftWord : begin Result := TLongIntField.Create(nil); Result.AsInteger := Value; end;
         ftBoolean     : begin Result := TBooleanField.Create(nil); Result.AsBoolean := Value; end;
         ftFloat       : begin Result := TFloatField.Create(nil);  Result.AsFloat := Value; end;
         ftCurrency    : begin end;
//         ftBCD         : begin Result := TBCDField.Create(nil); Result.Value := VarAsTyp(Value,varBCD); end;
         ftDate, ftDateTime, ftTime, ftTimeStamp :
            begin Result := TDateTimeField.Create(nil); Result.AsDateTime := VarToDateTime(Value); end;
         ftBytes       : begin Result := TBytesField.Create(nil); Result.Value := Value; end; //todo: fix
         ftVarBytes    : begin Result := TVarBytesField.Create(nil); Result.Value := Value; end; //todo: fix
         ftAutoInc     : begin Result := TAutoIncField.Create(nil); Result.Value := Value; end; //todo: check
         ftBlob        : begin Result := TBlobField.Create(nil); Result.Value := Value; end; //todo: fix
         ftMemo        : begin Result := TMemoField.Create(nil); Result.Value := Value; end; //todo: fix
         ftGraphic     : begin Result := TGraphicField.Create(nil); Result.Value := Value; end; //todo: fix
         ftFmtMemo     : begin end;
         ftParadoxOle  : begin end;
         ftDBaseOle    : begin end;
         ftTypedBinary : begin end;
         ftCursor      : Result := nil;
         ftFixedChar   : begin end;
         ftWideString  : begin end;
         ftLargeint    : begin Result := TLargeintField.Create(nil); Result.AsLongint := Value; end;
         ftADT         : begin end;
         ftArray       : begin end;
         ftReference   : begin end;
         ftDataSet     : begin end;
         ftOraBlob     : begin end;
         ftOraClob     : begin end;
         ftVariant     : begin end;
         ftInterface   : begin end;
         ftIDispatch   : begin end;
         ftGuid        : begin end;
         ftFMTBcd      : begin end;
         else Result := nil;
       end; // case
*)
(*
       if (Result <> nil) then
          Result.DataSet := FParentDataSet;
*)
     end;
end;

function TXMLBuffer.GetFieldByIndex(i: Integer): TField;
begin
   Result := FFields.Fields[i];
end;

procedure TXMLBuffer.CreateFields;
var i : Integer;
begin
(* Child node is the <field> element
<row>
  <field name="ID" value="0" datatype="integer" />
  <field name="NAME" size="8" datatype="string">
    <![CDATA[ Bulgaria ]]>
  </field>
</row>
*)
  FFields.Clear;
  if Assigned(FXMLNode) then
     for i := 0 to FXMLNode.ChildNodes.Count-1 do
         FFields.Add(CreateFieldFromXML(FXMLNode.ChildNodes.Item[i]));
end;

procedure TXMLBuffer.SetXMLNode(const ANode: TDOMNode);
begin
  FXMLNode := ANode;
  CreateFields;
end;

constructor TXMLBuffer.Create;
begin
  FXMLNode := TDOMNode.Create(nil);
  FFields  := TFields.Create(nil); // todo: should we have a dataset here
end;

constructor TXMLBuffer.Create(ADOMNode: TDOMNode);
begin
 Self.Create;
 FXMLNode := ADOMNode;
 CreateFields;
end;

destructor TXMLBuffer.Destroy;
begin
  FXMLNode.Free;
  FFields.Free;
  inherited Destroy;
end;

function TXMLBuffer.FieldByName(const AFieldName: String): TField;
begin
  Result := FFields.FieldByName(AFieldName);
end;

end.

