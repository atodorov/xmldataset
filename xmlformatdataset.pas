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
  Classes, SysUtils, DB, DOM;

const

// XML attribute values constants
  cYes   = 'yes';
  cNo    = 'no';
  cTrue  = 'true';
  cFalse = 'false';
  
// XML node and attribute name constants

// <datapacket version="2.000" destination="" day="28" month="8" year="2005" hour="20" min="57" sec="12" msec="46">
  cDatapacket             = 'datapacket';
  cDatapacket_Version     = 'version';
  cDatapacket_Destination = 'destination';
  cDatapacket_Day         = 'day';
  cDatapacket_Month       = 'month';
  cDatapacket_Year        = 'year';
  cDatapacket_Hour        = 'hour';
  cDatapacket_Min         = 'min';
  cDatapacket_Sec         = 'sec';
  cDatapacket_MSec        = 'msec';
  
// <producer name="alexx" url="" description="written by hand" />
  cProducer             = 'producer';
  cProducer_Name        = 'name';
  cProducer_URL         = 'url';
  cProducer_Description = 'description';
  
// <metadata fielddefs="yes" indexdefs="no" recorddata="yes" changes="yes">
  cMetadata            = 'metadata';
  cMetadata_FieldDefs  = 'fielddefs';
  cMetadata_IndexDefs  = 'indexdefs';
  cMetadata_RecordData = 'recorddata';
  cMetadata_Changes    = 'changes';

// <fielddefs count="2">
  cFieldDefs       = 'fielddefs';
  cFieldDefs_Count = 'count';
  
// <fielddef name="ID" fieldkind="data" datatype="integer" fieldsize="0" displaylabel="ID" displaywidth="10" fieldindex="0" required="true" readonly="false"/>
  cFieldDef              = 'fielddef';
  cFieldDef_Name         = 'name';
  cFieldDef_FieldKind    = 'fieldkind';
  cFieldDef_DataType     = 'datatype';
  cFieldDef_FieldSize    = 'fieldsize';
  cFieldDef_DisplayLabel = 'displaylabel';
  cFieldDef_DisplayWidth = 'displaywidth';
  cFieldDef_FieldIndex   = 'fieldindex';
  cFieldDef_Required     = 'required';
  cFieldDef_ReadOnly     = 'readonly';

// <indexdefs count="0" />
  cIndexDefs       = 'indexdefs';
  cIndexDefs_Count = 'count';
  
// <recorddata count="4" />
  cRecordData       = 'recorddata';
  cRecordData_Count = 'count';
  
// <row>
  cRow = 'row';
  
// <field name="ID" value="0" datatype="integer" />
  cField          = 'field';
  cField_Name     = 'name';
  cField_Value    = 'value';
  cField_DataType = 'datatype';
  cField_Size     = 'size';

type

  { forward declarations }
  TXMLFormatDataSet = class;

  { TXMLBuffer }

  PXMLBuffer = ^TXMLBuffer;

  TXMLBuffer = class (TObject)
  private
    FXMLElement : TDOMElement; // holds the xml value for a single record
    FBookmarkFlag : TBookmarkFlag;
//    FBookmark : Integer;
    FRecordNumber : Integer;
  protected
    procedure SetXMLElement(const ANode : TDOMElement);
  public
    constructor Create; virtual; overload;
    constructor Create(const ADOMElement : TDOMElement); virtual; overload;
    destructor  Destroy; override;
    function FieldByName(AFieldName : String) :TDOMElement;
    property BookmarkFlag : TBookmarkFlag read FBookmarkFlag write FBookmarkFlag;
//    property Fields[i : Integer] : TField read GetFieldByIndex;
    property RecordNumber : Integer read FRecordNumber write FRecordNumber;
    property XML : TDOMElement read FXMLElement write SetXMLElement;
//    property BookMark : Integer read FBookmark write FBookmark;
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
  function GetFieldSizeFromXML(FieldNode : TDOMElement; const Size : Integer = 0) : Integer;

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

function GetFieldSizeFromXML(FieldNode : TDOMElement; const Size : Integer = 0) : Integer;
begin
   Result := Size;
   if (FieldNode <> nil) and (FieldNode.AttribStrings[cField_Size] <> '') then
      Result := StrToInt(FieldNode.AttribStrings[cField_Size]);
end;


{ TXMLFormatDataSet }
constructor TXMLFormatDataSet.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FRecordSize   := SizeOf(TXMLBuffer);
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

// create field definitions
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

  FieldDefs.Clear;
  for i := 0 to FXMLDoc.DocumentElement.FindNode(cMetaData).FindNode(cFieldDefs).ChildNodes.Count - 1 do
      begin   // Add fields
        domNode := FXMLDoc.DocumentElement.FindNode(cMetadata).FindNode(cFieldDefs).ChildNodes.Item[i];
        FieldName := Trim(domNode.Attributes.GetNamedItem(cFieldDef_Name).NodeValue);
        ftFieldType := GetFieldTypeFromString(Trim(domNode.Attributes.GetNamedItem(cFieldDef_DataType).NodeValue));
        Required := AnsiUpperCase(domNode.Attributes.GetNamedItem(cFieldDef_Required).NodeValue) = 'TRUE';
        // determine field size
        FieldSize := GetFieldSizeByType(ftFieldType,StrToInt(domNode.Attributes.GetNamedItem(cFieldDef_FieldSize).NodeValue));

        FieldDefs.Add(FieldName, ftFieldType, FieldSize, Required);
        FieldDefs.Items[i].DisplayName := domNode.Attributes.GetNamedItem(cFieldDef_DisplayLabel).NodeValue;
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
//todo: what and where are bookmarks ??
  BookmarkSize := SizeOf(Integer);
//todo: fix this when bookmarks are constructed.
  FLastBookmark := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Count;
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
begin
  Result := Pointer(TXMLBuffer.Create);
end;

procedure TXMLFormatDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
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
// returns TXMLBuffer object into Buffer
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
        Move(
           Pointer(TXMLBuffer.Create(
                   TDOMElement(FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec])
                   ))^,
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
    FieldElement : TDOMElement;
    intValue : Integer;
    dblValue : Double;
    boolValue : Boolean;
    dtValue : TDateTime;
    strValue : String;
begin
  Result := GetActiveRecBuf(RecBuf);

  if Result and (RecBuf <> nil) then
     begin
       FieldElement := TXMLBuffer(Pointer(Buffer)^).FieldByName(Field.FieldName);
//******************************************************************************
       if FieldElement = nil then exit; // todo : fix this. must be removed
//******************************************************************************
       strValue := FieldElement.AttribStrings[cField_Value];
       
       case GetFieldTypeFromString(FieldElement.AttribStrings[cField_DataType]) of
         ftUnknown     : ;
         ftString      :
            begin
              if FTrimSpace then
                 strValue := Trim(strValue);
//todo : N.B. 0 or 1 based index
              Move(strValue[1],RecBuf[0],Length(strValue));
{$IFDEF DEBUGXML}
//ShowMessage('Value = '+IntToStr(intValue));
//ShowMessage('Buffer = '+IntToStr(Integer(Buffer^)));
{$ENDIF}
            end;
         ftSmallint, ftInteger, ftLargeint :
            begin
               intValue := StrToInt(strValue);
               Move(intValue,RecBuf^,SizeOf(intValue));
{$IFDEF DEBUGXML}
//ShowMessage('Value = '+IntToStr(intValue));
//ShowMessage('Buffer = '+IntToStr(Integer(Buffer^)));
{$ENDIF}
            end;
         ftWord        : ;
         ftBoolean     : begin boolValue := StrToBool(strValue); Move(boolValue,RecBuf^,SizeOf(boolValue)); end;
         ftFloat       : begin dblValue := StrToFloat(strValue); Move(dblValue,RecBuf^,SizeOf(dblValue));   end;
         ftCurrency    : ;
         ftBCD         : ;
         ftDate, ftTime, ftDateTime, ftTimeStamp :
             begin dtValue := StrToDateTime(strValue); Move(dtValue,RecBuf^,SizeOf(dtValue)); end;
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
       end;
     end;
end;

procedure TXMLFormatDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf, BufEnd: PChar;
  p : Integer;
begin
//todo: do it like GetFieldData when it gets to work
(*
  if not (State in [dsEdit, dsInsert]) then
     DatabaseError('Dataset not in edit or insert mode', Self);
  GetActiveRecBuf(RecBuf);

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
    then FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FCurRec].NodeValue := TXMLBuffer(Pointer(ActiveBuffer)^).XML.NodeValue
    else InternalAddRecord(ActiveBuffer, FALSE);
end;

procedure TXMLFormatDataSet.InternalEdit;
begin
  raise Exception.Create('TXMLFormatDataSet.InternalEdit');
end;

procedure TXMLFormatDataSet.InternalDelete;
begin
//todo: do not delete from XML. just mark record as deleted
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

    if DoAppend then
       InternalLast;

    NewChild := TDOMNode.Create(FXMLDoc);
    NewChild.NodeValue := TXMLBuffer(Pointer(Buffer)^).XML.NodeValue;

    RefChild := FXMLDoc.DocumentElement.FindNode(cRecordData).ChildNodes.Item[FLastBookmark];

    if DoAppend
      then FXMLDoc.DocumentElement.FindNode(cRecordData).AppendChild(NewChild)
      else FXMLDoc.DocumentElement.FindNode(cRecordData).InsertBefore(NewChild, RefChild);
  finally
    NewChild.Free;   // todo: clear or not ??
    RefChild := nil; // clear reference
  end;
end;

procedure TXMLFormatDataSet.InternalGotoBookmark(ABookmark: Pointer);
var Index: Integer;
begin
  Index := Integer(ABookmark^);
  if (Index <> -1) and (Index < RecordCount)
    then FCurRec := Index
    else DatabaseError('Bookmark not found');
end;

procedure TXMLFormatDataSet.InternalSetToRecord(Buffer: PChar);
begin
  if (State <> dsInsert) then
     InternalGotoBookmark(@TXMLBuffer(Pointer(Buffer)^).RecordNumber);
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
  Move(TXMLBuffer(Pointer(Buffer)^).RecordNumber, Data, BookmarkSize);
end;

procedure TXMLFormatDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin // BookmarkSize = ??
  Move(Data, TXMLBuffer(Pointer(Buffer)^).RecordNumber, BookmarkSize);
end;

{ TXMLBuffer }

procedure TXMLBuffer.SetXMLElement(const ANode: TDOMElement);
begin
  FXMLElement := ANode;
end;

constructor TXMLBuffer.Create;
begin
  FXMLElement := TDOMElement.Create(nil);
  FRecordNumber := -1;
  FBookmarkFlag := bfBOF;
end;

constructor TXMLBuffer.Create(const ADOMElement: TDOMElement);
begin
 Self.Create;
 FXMLElement := ADOMElement;
{$IFDEF DEBUGXML}
// ShowMessage('Name = '+FXMLElement.NodeName);
{$ENDIF}
end;

destructor TXMLBuffer.Destroy;
begin
  FXMLElement.Free;
  inherited Destroy;
end;

function TXMLBuffer.FieldByName(AFieldName: String): TDOMElement;
var i : Integer;
begin
(* Result is the <field> element
<row>
  <field name="ID"   value="0"        datatype="integer" />
  <field name="NAME" value="Bulgaria" datatype="string"  size="8" />
</row>
*)
  Result := nil;
//******************************************************************************
  if not Assigned(FXMLElement) then exit; // todo: fix this
//******************************************************************************
  AFieldName := AnsiUpperCase(AFieldName);
{$IFDEF DEBUGXML}
//ShowMessage('RecNo = '+IntToStr(FRecordNumber));
//ShowMessage('Name = '+FXMLElement.NodeName);
{$ENDIF}

  for i := 0 to FXMLElement.ChildNodes.Count - 1 do
    if (AnsiUpperCase(FXMLElement.ChildNodes.Item[i].Attributes.GetNamedItem(cField_Name).NodeValue) = AFieldName) then
       begin
         Result := TDOMElement(FXMLElement.ChildNodes.Item[i]);
         exit;
       end;
end;

end.

