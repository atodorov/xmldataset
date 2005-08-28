unit XMLFormatDataSet;

{$mode objfpc}{$H+}

interface

uses
  Db, Classes, SysUtils;

const
  MAXSTRLEN = 250;


type

//------------------------------------------------------------------------------
// TRecInfo
  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    RecordNumber: PtrInt;
    BookmarkFlag: TBookmarkFlag;
  end;

//------------------------------------------------------------------------------
// TXMLFormatDataSet
  TXMLFormatDataSet = class(TDataSet)
  private
    FSchema             :TStringList;
    FFileName           :TFileName;
    FFilterBuffer       :PChar;
    FFileMustExist      :Boolean;
    FReadOnly           :Boolean;
    FLoadfromStream     :Boolean;
    FTrimSpace          :Boolean;
    procedure SetSchema(const Value: TStringList);
    procedure SetFileName(Value : TFileName);
    procedure SetFileMustExist(Value : Boolean);
    procedure SetTrimSpace(Value : Boolean);
    procedure SetReadOnly(Value : Boolean);
    procedure RemoveWhiteLines(List : TStrings; IsFileRecord : Boolean);
    procedure LoadFieldScheme(List : TStrings; MaxSize : Integer);
    function GetActiveRecBuf(var RecBuf: PChar): Boolean;
    procedure SetFieldPos(var Buffer : PChar; FieldNo : Integer);
  protected
    FData               :TStringlist;
    FCurRec             :Integer;
    FRecBufSize         :Integer;
    FRecordSize         :Integer;
    FLastBookmark       :PtrInt;
    FRecInfoOfs         :Word;
    FBookmarkOfs        :Word;
    FSaveChanges        :Boolean;
  protected
    function AllocRecordBuffer: PChar; override;
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
    function IsCursorOpen: Boolean; override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure ClearCalcFields(Buffer: PChar); override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetCanModify: boolean; override;
    function TxtGetRecord(Buffer : PChar; GetMode: TGetMode): TGetResult;
    function RecordFilter(RecBuf: Pointer; ARecNo: Integer): Boolean;
    function BufToStore(Buffer: PChar): String; virtual;
    function StoreToBuf(Source: String): String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure RemoveBlankRecords; dynamic;
    procedure RemoveExtraColumns; dynamic;
    procedure SaveFileAs(strFileName : String); dynamic;
    property  CanModify;
    procedure LoadFromStream(Stream :TStream);
    procedure SavetoStream(Stream :TStream);
  published
    property FileMustExist: Boolean read FFileMustExist write SetFileMustExist;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property FileName : TFileName read FFileName write SetFileName;
    property Schema: TStringList read FSchema write SetSchema;
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

implementation

//------------------------------------------------------------------------------
// TXMLFormatDataSet
//------------------------------------------------------------------------------
constructor TXMLFormatDataSet.Create(AOwner : TComponent);
begin
 FFileMustExist  := TRUE;
 FLoadfromStream := False;
 FRecordSize   := 0;
 FTrimSpace     := TRUE;
 FSchema       := TStringList.Create;
 FData         := TStringList.Create;  // Load the textfile into a stringlist
 inherited Create(AOwner); // this maybe should go on top
end;

destructor TXMLFormatDataSet.Destroy;
begin
 FData.Free;
 FSchema.Free;
 inherited Destroy;
end;

procedure TXMLFormatDataSet.SetSchema(const Value: TStringList);
begin
  CheckInactive;
  FSchema.Assign(Value);
end;

procedure TXMLFormatDataSet.SetFileMustExist(Value : Boolean);
begin
  CheckInactive;
  FFileMustExist := Value;
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

procedure TXMLFormatDataSet.SetFileName(Value : TFileName);
begin
  CheckInactive;
  FFileName := Value;
end;

// create field definitions based on current schema
procedure TXMLFormatDataSet.InternalInitFieldDefs;
var
  i, len, Maxlen :Integer;
  LstFields      :TStrings;
begin
  if not Assigned(FData) then
    exit;
  FRecordSize := 0;
  Maxlen := 0;
  FieldDefs.Clear;
  for i := FData.Count - 1 downto 0 do  // Find out the longest record
  begin
    len := Length(FData[i]);
    if len > Maxlen then
      Maxlen := len;
    FData.Objects[i] := TObject(Pointer(i+1));   // Fabricate Bookmarks
  end;
  if (Maxlen = 0) then
    Maxlen := MAXSTRLEN;
  LstFields := TStringList.Create;
  try
    LoadFieldScheme(LstFields, Maxlen);
    for i := 0 to LstFields.Count -1 do  // Add fields
    begin
      len := StrToIntDef(LstFields.Values[LstFields.Names[i]], Maxlen);
      FieldDefs.Add(Trim(LstFields.Names[i]), ftString, len, False);
      Inc(FRecordSize, len);
    end;
  finally
    LstFields.Free;
  end;
end;

procedure TXMLFormatDataSet.InternalOpen;
var
  Stream : TStream;
begin
  FCurRec := -1;
  FSaveChanges := FALSE;
  if not Assigned(FData) then
    FData := TStringList.Create;
  if (not FileMustExist) and (not FileExists(FileName)) then
  begin
    Stream := TFileStream.Create(FileName, fmCreate);
    Stream.Free;
  end;
  if not FLoadfromStream then
    FData.LoadFromFile(FileName);
  FRecordSize := MAXSTRLEN;
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields(TRUE);
  if FRecordSize = 0 then
    FRecordSize := MAXSTRLEN;
  BookmarkSize := SizeOf(Integer);
  FRecInfoOfs := FRecordSize + CalcFieldsSize; // Initialize the offset for TRecInfo in the buffer
  FBookmarkOfs := FRecInfoOfs + SizeOf(TRecInfo);
  FRecBufSize := FBookmarkOfs + BookmarkSize;
  FLastBookmark := FData.Count;
end;

procedure TXMLFormatDataSet.InternalClose;
begin
  if (not FReadOnly) and (FSaveChanges) then  // Write any edits to disk
    FData.SaveToFile(FileName);
  FLoadfromStream := False;
  FData.Clear;
  BindFields(FALSE);
  if DefaultFields then // Destroy the TField
    DestroyFields;
  FCurRec := -1;        // Reset these internal flags
  FLastBookmark := 0;
  FRecordSize := 0;
end;

function TXMLFormatDataSet.IsCursorOpen: Boolean;
begin
  Result := Assigned(FData) and (FRecordSize > 0);
end;

procedure TXMLFormatDataSet.InternalHandleException;
begin
{$ifndef fpc}
   Application.HandleException(Self);
{$endif}
end;

// Loads Data from a stream.
procedure TXMLFormatDataSet.LoadFromStream(Stream: TStream);
begin
  if assigned(stream) then
  begin
    Active          := False; //Make sure the Dataset is Closed.
    Stream.Position := 0;     //Make sure you are at the top of the Stream.
    FLoadfromStream := True;
    if not Assigned(FData) then
     raise Exception.Create('Data buffer unassigned');
    FData.LoadFromStream(Stream);
    Active := True;
  end
  else
    raise exception.Create('Invalid Stream Assigned (Load From Stream');
end;

// Saves Data as text to a stream.
procedure TXMLFormatDataSet.SavetoStream(Stream: TStream);
begin
  if assigned(stream) then
    FData.SaveToStream(Stream)
  else
    raise exception.Create('Invalid Stream Assigned (Save To Stream');
end;

// Record Functions
function TXMLFormatDataSet.AllocRecordBuffer: PChar;
begin
  if FRecBufSize > 0 then
    Result := AllocMem(FRecBufSize)
  else
    Result := nil;
end;

procedure TXMLFormatDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
  if Buffer <> nil then
    FreeMem(Buffer);
end;

procedure TXMLFormatDataSet.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer[0], FRecordSize, 0);
end;

procedure TXMLFormatDataSet.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[RecordSize], CalcFieldsSize, 0);
end;

function TXMLFormatDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  if (FData.Count < 1) then
    Result := grEOF
  else
    Result := TxtGetRecord(Buffer, GetMode);
  if Result = grOK then
  begin
    if (CalcFieldsSize > 0) then
      GetCalcFields(Buffer);
    with PRecInfo(Buffer + FRecInfoOfs)^ do
    begin
      BookmarkFlag := bfCurrent;
      RecordNumber := PtrInt(FData.Objects[FCurRec]);
    end;
  end
  else
    if (Result = grError) and DoCheck then
      DatabaseError('No Records');
end;

function TXMLFormatDataSet.GetRecordCount: Longint;
begin
  Result := FData.Count;
end;

function TXMLFormatDataSet.GetRecNo: Longint;
var
  BufPtr: PChar;
begin
  Result := -1;
  if GetActiveRecBuf(BufPtr) then
    Result := PRecInfo(BufPtr + FRecInfoOfs)^.RecordNumber;
end;

procedure TXMLFormatDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value >= 0) and (Value < FData.Count) and (Value <> RecNo) then
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
    dsBrowse: if IsEmpty then RecBuf := nil else RecBuf := ActiveBuffer;
    dsEdit, dsInsert: RecBuf := ActiveBuffer;
    dsCalcFields: RecBuf := CalcBuffer;
    dsFilter: RecBuf := FFilterBuffer;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

function TXMLFormatDataSet.TxtGetRecord(Buffer : PChar; GetMode: TGetMode): TGetResult;
var
  Accepted : Boolean;
begin
  Result := grOK;
  repeat
    Accepted := TRUE;
    case GetMode of
      gmNext:
        if FCurRec >= RecordCount - 1  then
          Result := grEOF
        else
          Inc(FCurRec);
      gmPrior:
        if FCurRec <= 0 then
          Result := grBOF
        else
          Dec(FCurRec);
      gmCurrent:
        if (FCurRec < 0) or (FCurRec >= RecordCount) then
          Result := grError;
    end;
    if (Result = grOk) then
    begin
      Move(PChar(StoreToBuf(FData[FCurRec]))^, Buffer[0], FRecordSize);
      if Filtered then
      begin
        Accepted := RecordFilter(Buffer, FCurRec +1);
        if not Accepted and (GetMode = gmCurrent) then
          Inc(FCurRec);
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
  PRecInfo(FFilterBuffer + FRecInfoOfs)^.RecordNumber := ARecNo;
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
procedure TXMLFormatDataSet.LoadFieldScheme(List : TStrings; MaxSize : Integer);
var
  tmpFieldName : string;
  tmpSchema : TStrings;
  i : Integer;
begin
  tmpSchema := TStringList.Create;
  try       // Load Schema Structure
    if (Schema.Count > 0) then
    begin
      tmpSchema.Assign(Schema);
      RemoveWhiteLines(tmpSchema, FALSE);
    end
    else
      tmpSchema.Add('Line');
    for i := 0 to tmpSchema.Count -1 do // Interpret Schema
    begin
      tmpFieldName := tmpSchema.Names[i];
      if (tmpFieldName = '') then
        tmpFieldName := Format('%s=%d', [tmpSchema[i], MaxSize])
      else
        tmpFieldName := tmpSchema[i];
      List.Add(tmpFieldName);
    end;
  finally
    tmpSchema.Free;
  end;
end;

function TXMLFormatDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  TempPos, RecBuf: PChar;
begin
  Result := GetActiveRecBuf(RecBuf);
  if Result then
  begin
    if Field.FieldNo > 0 then
    begin
      TempPos := RecBuf;
      SetFieldPos(RecBuf, Field.FieldNo);
      Result := (RecBuf < StrEnd(TempPos));
    end
    else
      if (State in [dsBrowse, dsEdit, dsInsert, dsCalcFields]) then
      begin
        Inc(RecBuf, FRecordSize + Field.Offset);
        Result := Boolean(Byte(RecBuf[0]));
      end;
  end;
  if Result and (Buffer <> nil) then
  begin
    StrLCopy(Buffer, RecBuf, Field.Size);
    if FTrimSpace then
    begin
      TempPos := StrEnd(Buffer);
      repeat
        Dec(TempPos);
        if (TempPos[0] = ' ') then
          TempPos[0]:= #0
        else
          break;
      until (TempPos = Buffer);
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
end;

procedure TXMLFormatDataSet.SetFieldPos(var Buffer : PChar; FieldNo : Integer);
var
  i : Integer;
begin
  i := 1;
  while (i < FieldNo) and (i < FieldDefs.Count) do
  begin
    Inc(Buffer, FieldDefs.Items[i-1].Size);
    Inc(i);
  end;
end;

// Navigation / Editing
procedure TXMLFormatDataSet.InternalFirst;
begin
  FCurRec := -1;
end;

procedure TXMLFormatDataSet.InternalLast;
begin
  FCurRec := FData.Count;
end;

procedure TXMLFormatDataSet.InternalPost;
var
  i: Longint;
begin
  FSaveChanges := TRUE;
  inherited UpdateRecord;
  if (State = dsEdit) then // just update the data in the string list
  begin
    FData[FCurRec] := BufToStore(ActiveBuffer);
  end
  else
    InternalAddRecord(ActiveBuffer, FALSE);
end;

procedure TXMLFormatDataSet.InternalEdit;
begin

end;

procedure TXMLFormatDataSet.InternalDelete;
begin
  FSaveChanges := TRUE;
  FData.Delete(FCurRec);
  if FCurRec >= FData.Count then
    Dec(FCurRec);
end;

procedure TXMLFormatDataSet.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);
begin
  FSaveChanges := TRUE;
  Inc(FLastBookmark);
  if DoAppend then
    InternalLast;
  if (FCurRec >=0) then
    FData.InsertObject(FCurRec, BufToStore(Buffer), TObject(Pointer(FLastBookmark)))
  else
    FData.AddObject(BufToStore(Buffer), TObject(Pointer(FLastBookmark)));
end;

procedure TXMLFormatDataSet.InternalGotoBookmark(ABookmark: Pointer);
var
  Index: Integer;
begin
  Index := FData.IndexOfObject(TObject(PPtrInt(ABookmark)^));
  if Index <> -1 then
    FCurRec := Index
  else
    DatabaseError('Bookmark not found');
end;

procedure TXMLFormatDataSet.InternalSetToRecord(Buffer: PChar);
begin
  if (State <> dsInsert) then
    InternalGotoBookmark(@PRecInfo(Buffer + FRecInfoOfs)^.RecordNumber);
end;

function TXMLFormatDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + FRecInfoOfs)^.BookmarkFlag;
end;

procedure TXMLFormatDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer + FRecInfoOfs)^.BookmarkFlag := Value;
end;

procedure TXMLFormatDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(Buffer[FBookmarkOfs], Data^, BookmarkSize);
end;

procedure TXMLFormatDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(Data^, Buffer[FBookmarkOfs], BookmarkSize);
end;

procedure TXMLFormatDataSet.RemoveWhiteLines(List : TStrings; IsFileRecord : Boolean);
var
  i : integer;
begin
  for i := List.Count -1 downto 0 do
  begin
    if (Trim(List[i]) = '' ) then
      if IsFileRecord then
      begin
        FCurRec := i;
        InternalDelete;
      end
      else
        List.Delete(i);
  end;
end;

procedure TXMLFormatDataSet.RemoveBlankRecords;
begin
  RemoveWhiteLines(FData, TRUE);
end;

procedure TXMLFormatDataSet.RemoveExtraColumns;
var
  i : Integer;
begin
  for i := FData.Count -1 downto 0 do
    FData[i] := BufToStore(PChar(StoreToBuf(FData[i])));
  FData.SaveToFile(FileName);
end;

procedure TXMLFormatDataSet.SaveFileAs(strFileName : String);
begin
  FData.SaveToFile(strFileName);
  FFileName := strFileName;
  FSaveChanges := FALSE;
end;

function TXMLFormatDataSet.StoreToBuf(Source: String): String;
begin
  Result := Source;
end;

function TXMLFormatDataSet.BufToStore(Buffer: PChar): String;
begin
  Result := Copy(Buffer, 1, FRecordSize);
end;

end.

