unit gxbasedataset;

{$mode objfpc}{$H+}
{$I xmldsdefs.inc}

{*******************************************************************************

 This file is part of the XMLDataset suite for the Free Component Library!

 This source code implements a base class for deriving custom datasets.
 It is described in the article "Writing Custom Datasets" available at
 http://www.gexperts.com/CustomDS.html

 GExperts Inc.
 www.gexperts.com

 FPC modifications and additional improvements made by Alexander Todorov.
 e-mail: alexx.todorov@gmail.com
 
 *****************************************************************************
 *                                                                           *
 *  Since I could not contact the original author and found that this source *
 *  is used in other TDataset implementations (see TjanXMLDataset)           *
 *  it is distributed under modified Library GNU General Public License!     *
 *  See the file COPYING included in this distribution,                      *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
*******************************************************************************}

// todo : TimeStampToMSecs / MSecsToTimeStamp doesn't work for date fields.
// TimeStamp issues are due to differences between FPC and Delphi. check them out.
// it is written in the mailing list about that.

interface

uses Classes, SysUtils, DB;

resourcestring
  FT_NOT_SUPPORTED = 'Field type not supported';
  
const
  (*** UNSUPPORTED FIELD TYPES ***)
  UNSUPPORTED = [
//  ftBlob, ftMemo, ftGraphic, // <--- BLOBS ARE NOT TESTED BUT ITS SUPPOSED TO WORK
    ftFmtMemo, ftUnknown, ftWord, ftBytes, ftVarBytes, ftAutoInc,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
    ftWideString, ftADT, ftArray, ftReference,  ftDataSet, ftOraBlob,
    ftOraClob, ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd];
  (*****************************************************************************
    To implement support for specific field type several thing must be done :
    1) add ftXXX to GetDataSize - this returns how many bytes are used to
       store a single value from the given data type. Most cases this is SizeOf(TypeXXX)
    2) add ftXXX to GetFieldOffset - this returns the field offset in the buffer structure.
       this is a sum from all fields preceding a particular field of type XXX
    3) add ftXXX to BufferToRecord / RecordToBuffer - these two procedures interact with
       the internal buffer system and the higher-level variant system.
       N.B. since these procedures call GetFieldData / SetFieldData the actual data
       must be fitted in a Variant.
    4) add ftXXX to GetFieldData / SetFieldData - these functions interact with the
       internal buffer system.
    5) remove ftXXX from UNSUPPORTED set
    
    6) CustomXMLDataSet which inherits from this one must be told how to handle new data
       and how it is represented in XML.
  *****************************************************************************)

type

  PRecordInfo = ^TRecordInfo;
  TRecordInfo = record
    RecordID: Pointer;
    Bookmark: Pointer;
    BookMarkFlag: TBookmarkFlag;
  end;

  TGXBaseDataset = class(TDataset)
  private
    FisOpen: Boolean;
    FStartCalculated: Integer;
    FBufferMap: TStringList;
    procedure FillBufferMap;
    function  RecordFilter: Boolean;
  protected   {My simplified methods to override}
    {$IFDEF VER2_0_1}
    {$WARNING This is a dirty hack! Be careful. NOT tested!}
    (* TDataset.TempBuffer is not present in FPC 2.0.1. It is present in older and newer releases *)
    (* It is implemented by TGXBaseDataSet for compatibility *)
    function TempBuffer: PChar;
    {$ENDIF}

    function  DoOpen: Boolean; virtual; abstract;
    procedure DoClose; virtual; abstract;
    procedure DoDeleteRecord; virtual;
    procedure DoCreateFieldDefs; virtual; abstract;
    function  GetFieldValue(Field: TField): Variant; virtual; abstract;
    procedure SetFieldValue(Field: TField; Value: Variant); virtual; abstract;
    procedure GetBlobField(Field: TField; Stream: TStream); virtual; abstract;
    procedure SetBlobField(Field: TField; Stream: TStream); virtual; abstract;
    //Called before and after getting a set of field values
    procedure DoBeforeGetFieldValue; virtual; abstract;
    procedure DoAfterGetFieldValue; virtual; abstract;
    procedure DoBeforeSetFieldValue(Inserting: Boolean); virtual; abstract;
    procedure DoAfterSetFieldValue(Inserting: Boolean); virtual; abstract;

    //Handle buffer ID
    function  AllocateRecordID: Pointer; virtual; abstract;
    procedure DisposeRecordID(Value: Pointer); virtual; abstract;
    procedure GotoRecordID(Value: Pointer); virtual; abstract;
    //BookMark functions
    function  GetBookMarkSize: Integer; virtual; abstract;
    procedure DoGotoBookmark(ABookmark: Pointer); virtual; abstract;
    procedure AllocateBookMark(RecordID: Pointer; ABookmark: Pointer); virtual; abstract;

    //Navigation methods
    procedure DoFirst; virtual; abstract;
    procedure DoLast; virtual; abstract;
    function  Navigate(GetMode: TGetMode): TGetResult; virtual; abstract;
    procedure SetFiltered(Value: Boolean); override;
    //Internal isOpen property
    property  isOpen: Boolean read FisOpen;

    { TGXBaseDataset Internal functions that can be overriden if needed }
    procedure AllocateBLOBPointers(Buffer: PChar); virtual;
    procedure FreeBlobPointers(Buffer: PChar); virtual;
    
    procedure FreeRecordPointers(Buffer: PChar); virtual;
    function  GetDataSize: Integer; virtual;
    function  GetFieldOffset(Field: TField): Integer; virtual;
    procedure BufferToRecord(Buffer: PChar); virtual;
    procedure RecordToBuffer(Buffer: PChar); virtual;

    function  AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function  GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize: Word; override;
    procedure InternalInsert; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalEdit; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure InternalAddRecord(Buffer: Pointer; bAppend: Boolean); override;
    function  IsCursorOpen: Boolean; override;
    function  GetCanModify: Boolean; override;
    procedure ClearCalcFields(Buffer: PChar); override;
    function  GetActiveRecordBuffer: PChar; virtual;
    function  GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function  GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    function  CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TGXBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TGXBaseDataSet;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FOpened: Boolean;
    procedure LoadBlobData;
    procedure SaveBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

var OldTimeSeparator, OldDateSeparator : Char;


{ TGXBaseDataset }

{$IFDEF VER2_0_1}
function TGXBaseDataset.TempBuffer : PChar;
begin
  Result := ActiveBuffer;
end;
{$ENDIF}
    
constructor TGXBaseDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBufferMap := TStringList.Create;
end;

destructor TGXBaseDataset.Destroy;
begin
  if Active then
    Close;
  FBufferMap.Free;
  inherited Destroy;
end;

procedure TGXBaseDataset.FillBufferMap;
var
  Index: Integer;
begin
  FBufferMap.Clear;
  for Index := 0 to FieldCount - 1 do
    FBufferMap.Add(Fields[Index].FieldName);
end;

procedure TGXBaseDataset.InternalOpen;
begin
  if DoOpen then
    begin
      BookmarkSize := GetBookMarkSize;
      InternalInitFieldDefs;
      if DefaultFields then
        CreateFields;
      BindFields(True);
      FisOpen := True;
      FillBufferMap;
    end;
end;

function TGXBaseDataset.AllocRecordBuffer: PChar;
begin
  GetMem(Result, GetRecordSize);
  FillChar(Result^, GetRecordSize, 0);
  AllocateBlobPointers(Result);
end;

procedure TGXBaseDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeRecordPointers(Buffer);
  FreeMem(Buffer, GetRecordSize);
end;

procedure TGXBaseDataset.FreeRecordPointers(Buffer: PChar);
begin
  FreeBlobPointers(Buffer);
  DisposeRecordID(PRecordInfo(Buffer + GetDataSize)^.RecordID);
  if PRecordInfo(Buffer + GetDataSize)^.BookMark <> nil then
    begin
      FreeMem(PRecordInfo(Buffer + GetDataSize)^.BookMark);
      PRecordInfo(Buffer + GetDataSize)^.BookMark := nil;
    end;
end;

procedure TGXBaseDataset.AllocateBLOBPointers(Buffer: PChar);
var
  Index: Integer;
  Offset: Integer;
  Stream: TMemoryStream;
begin
  for Index := 0 to FieldCount - 1 do
    if Fields[Index].IsBlob then
      begin
        Offset := GetFieldOffset(Fields[Index]);
        Stream := TMemoryStream.Create;
        Move(Pointer(Stream), (Buffer + Offset)^, SizeOf(Pointer));
      end;
end;

procedure TGXBaseDataset.FreeBlobPointers(Buffer: PChar);
var
  Index: Integer;
  Offset: Integer;
  FreeObject: TObject;
begin
  for Index := 0 to FieldCount - 1 do
    if Fields[Index].IsBlob then
      begin
        Offset := GetFieldOffset(Fields[Index]);
        Move((Buffer + Offset)^, Pointer(FreeObject), SizeOf(Pointer));
        if Assigned(FreeObject) then
           FreeObject.Free;
        FreeObject := nil;
        Move(Pointer(FreeObject), (Buffer + Offset)^, SizeOf(Pointer));
      end;
end;

procedure TGXBaseDataset.InternalInitFieldDefs;
begin
  DoCreateFieldDefs;
end;

procedure TGXBaseDataset.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[FStartCalculated], CalcFieldsSize, 0);
end;

function TGXBaseDataset.GetActiveRecordBuffer: PChar;
begin
  case State of
    dsBrowse: if isEmpty then
        Result := nil
      else
        Result := ActiveBuffer;
    dsCalcFields: Result := CalcBuffer;
    dsFilter: Result := TempBuffer;
    dsEdit, dsInsert: Result := ActiveBuffer;
  else
    Result := nil;
  end;
end;

function TGXBaseDataset.GetCanModify: Boolean;
begin
  Result := False;
end;

function TGXBaseDataset.RecordFilter: Boolean;
var
  SaveState: TDataSetState;
begin
  Result := True;
  if Assigned(OnFilterRecord) then
    begin
      SaveState := SetTempState(dsFilter);
      try
        RecordToBuffer(TempBuffer);
        OnFilterRecord(Self, Result);
      except
      {$IFNDEF FPC}
        Application.HandleException(Self);
      {$ENDIF}
      end;
      RestoreState(SaveState);
    end;
end;

function TGXBaseDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  localAccept : boolean;
begin
  localAccept := True;
  repeat
    Result := Navigate(GetMode);
    if (Result = grOk) then
      begin
        if Filtered then
          localAccept := RecordFilter;

        if localAccept then
          begin
            RecordToBuffer(Buffer);
            ClearCalcFields(Buffer);
            GetCalcFields(Buffer);
          end;
      end
    else if (Result = grError) and DoCheck then
      DatabaseError('No Records');
  until localAccept or (Result in [grEOF, grBOF]);
end;

function TGXBaseDataset.GetRecordSize: Word;
var tmp : Word;
begin
  tmp := GetDataSize + SizeOf(TRecordInfo);

  Result := tmp + CalcFieldsSize;
  FStartCalculated := tmp;
end;

function TGXBaseDataset.GetDataSize: Integer;
var Index : Integer;
begin
  Result := 0;
  for Index := 0 to FieldCount - 1 do
    case Fields[Index].DataType of
      ftString: Result := Result + Fields[Index].Size + 1; //Leave space for terminating null
      ftInteger, ftSmallInt, ftDate, ftTime  : Result := Result + SizeOf(Integer);
      ftLargeInt : Result := Result + SizeOf(LargeInt);
      ftFloat, ftCurrency, ftBCD, ftDateTime : Result := Result + SizeOf(Double);
      ftBoolean: Result := Result + SizeOf(WordBool);
      else if Fields[Index].IsBlob
              then Result := Result + SizeOf(Pointer)
              else if (Fields[Index].DataType in UNSUPPORTED) then
                      raise Exception.Create('TGXBaseDataset.GetDataSize - '+FT_NOT_SUPPORTED);
    end;
end;

procedure TGXBaseDataset.InternalClose;
begin
  BindFields(False);
  if DefaultFields then
    DestroyFields;
  DoClose;
  FisOpen := False;
end;

procedure TGXBaseDataset.InternalDelete;
begin
  DoDeleteRecord;
end;

procedure TGXBaseDataset.InternalEdit;
begin
  if GetActiveRecordBuffer <> nil then
     InternalSetToRecord(GetActiveRecordBuffer);
end;

procedure TGXBaseDataset.InternalFirst;
begin
  DoFirst;
end;

procedure TGXBaseDataset.InternalHandleException;
begin
{$IFNDEF FPC}
  Application.HandleException(Self);
{$ENDIF}
end;

{This is called by the TDataset to initialize an already existing buffer.
We cannot just fill the buffer with 0s since that would overwrite our BLOB pointers.
Therefore we free the blob pointers first, then fill the buffer with zeros, then
reallocate the blob pointers}

procedure TGXBaseDataset.InternalInitRecord(Buffer: PChar);
begin
  FreeRecordPointers(Buffer);
  FillChar(Buffer^, GetRecordSize, 0);
  AllocateBlobPointers(Buffer);
end;

procedure TGXBaseDataset.InternalInsert;
begin

end;

procedure TGXBaseDataset.InternalLast;
begin
  DoLast;
end;

procedure TGXBaseDataset.InternalPost;
begin
  if FisOpen then
    begin
      DoBeforeSetFieldValue(State = dsInsert);
      BufferToRecord(GetActiveRecordBuffer);
      DoAfterSetFieldValue(State = dsInsert);
    end;
end;

procedure TGXBaseDataset.InternalAddRecord(Buffer: Pointer; bAppend: Boolean);
begin
  if bAppend then
    InternalLast;
  DoBeforeSetFieldValue(True);
  BufferToRecord(Buffer);
  DoAfterSetFieldValue(True);
end;

procedure TGXBaseDataset.InternalSetToRecord(Buffer: PChar);
begin
  GotoRecordID(PRecordInfo(Buffer + GetDataSize)^.RecordID);
end;

function TGXBaseDataset.IsCursorOpen: Boolean;
begin
  Result := FisOpen;
end;

function TGXBaseDataset.GetFieldOffset(Field: TField): Integer;
var Index, FPos : Integer;
begin
  Result := 0;
  FPos := FBufferMap.Indexof(Field.FieldName);
  for Index := 0 to FPos - 1 do
    case FieldByName(FBufferMap[Index]).DataType of
      ftString : inc(Result, FieldbyName(FBufferMap[Index]).Size + 1);
      ftInteger, ftSmallInt, ftDate, ftTime : inc(Result, SizeOf(Integer));
      ftLargeInt : inc(Result, SizeOf(LargeInt));
      ftDateTime, ftFloat, ftBCD, ftCurrency: inc(Result, SizeOf(Double));
      ftBoolean: inc(Result, SizeOf(WordBool));
      else if Fields[Index].IsBlob
              then Result := Result + SizeOf(Pointer)
              else if (FieldByName(FBufferMap[Index]).DataType in UNSUPPORTED) then
                      raise Exception.Create('TGXBaseDataset.GetFieldOffset - '+FT_NOT_SUPPORTED);
    end;
end;

procedure TGXBaseDataset.BufferToRecord(Buffer: PChar);
var
  TempStr: string;
  TempInt: Integer;
  TempLInt : LargeInt;
  TempDouble: Double;
  TempBool: WordBool;
  Offset: Integer;
  Index: Integer;
  Stream: TStream;
begin
  for Index := 0 to FieldCount - 1 do
    begin
      Offset := GetFieldOffset(Fields[Index]);
      case Fields[Index].DataType of
        ftString:
          begin
            TempStr := PChar(Buffer + Offset);
            SetFieldValue(Fields[Index], TempStr);
          end;
        ftInteger, ftSmallInt, ftDate, ftTime:
          begin
            Move((Buffer + Offset)^, TempInt, SizeOf(Integer));
            SetFieldValue(Fields[Index], TempInt);
          end;
        ftLargeInt :
          begin
            Move((Buffer + Offset)^, TempLInt, SizeOf(LargeInt));
            SetFieldValue(Fields[Index], TempLInt);
          end;
        ftFloat, ftBCD, ftCurrency :
          begin
            Move((Buffer + Offset)^, TempDouble, SizeOf(Double));
            SetFieldValue(Fields[Index], TempDouble);
          end;
        ftDateTime :
          begin
            Move((Buffer + Offset)^, TempDouble, SizeOf(Double));
            SetFieldValue(Fields[Index], DateTimeToStr(TempDouble));
          end;
        ftBoolean:
          begin
            Move((Buffer + Offset)^, TempBool, SizeOf(WordBool));
            SetFieldValue(Fields[Index], BoolToStr(TempBool));
//            SetFieldValue(Fields[Index], TempBool);
          end;
        else if Fields[Index].IsBlob then
                begin
                  Move((Buffer + Offset)^, Pointer(Stream), sizeof(Pointer));
                  Stream.Position := 0;
                  SetBlobField(Fields[Index], Stream);
                end
             else if (Fields[Index].DataType in UNSUPPORTED) then
                     raise Exception.Create('TGXBaseDataset.BufferToRecord - '+FT_NOT_SUPPORTED);
      end;
    end;
end;

procedure TGXBaseDataset.RecordToBuffer(Buffer: PChar);
var
  Value: Variant;
  TempStr: string;
  TempInt: Integer;
  TempLInt : LargeInt;
  TempDouble: Double;
  TempBool: WordBool;
  Offset: Integer;
  Index: Integer;
  Stream: TStream;
begin
  with PRecordInfo(Buffer + GetDataSize)^ do
    begin
      BookmarkFlag := bfCurrent;
      RecordID := AllocateRecordID;
      if GetBookMarkSize > 0 then
        begin
          if BookMark = nil then
             GetMem(BookMark, GetBookMarkSize);
          AllocateBookMark(RecordID, BookMark);
        end
      else
        BookMark := nil;
    end;
  DoBeforeGetFieldValue;
  for Index := 0 to FieldCount - 1 do
    begin
      if not Fields[Index].IsBlob then
         Value := GetFieldValue(Fields[Index]);
      Offset := GetFieldOffset(Fields[Index]);
      case Fields[Index].DataType of
        ftString:
          begin
            TempStr := Value;
            if length(TempStr) > Fields[Index].Size then
              System.Delete(TempStr, Fields[Index].Size, length(TempStr) - Fields[Index].Size);
            StrLCopy(PChar(Buffer + Offset), PChar(TempStr), length(TempStr));
          end;
        ftInteger, ftSmallInt, ftDate, ftTime:
          begin
            TempInt := Value;
            Move(TempInt, (Buffer + Offset)^, SizeOf(TempInt));
          end;
        ftLargeInt :
          begin
            {$IFDEF USE_STR_TO_INT64}
            TempLInt := StrToInt64(Value);
            {$ELSE}
            TempLInt := Value;
            {$ENDIF}
            Move(TempLInt, (Buffer + Offset)^, SizeOf(TempLInt));
          end;
        ftFloat, ftBCD, ftCurrency:
          begin
            {$IFDEF USE_STR_TO_FLOAT}
            TempDouble := StrToFloat(Value);
            {$ELSE}
            TempDouble := Value;
            {$ENDIF}
            Move(TempDouble, (Buffer + Offset)^, SizeOf(TempDouble));
          end;
        ftDateTime :
          begin  // convert Variant (string) to TDateTime and write it to buffer
            TempDouble := StrToDateTime(Value);
            Move(TempDouble, (Buffer + Offset)^, SizeOf(TempDouble));
          end;
        ftBoolean:
          begin
            TempBool := StrToBool(Value);
            Move(TempBool, (Buffer + Offset)^, SizeOf(TempBool));
          end;
        else if Fields[Index].IsBlob then
                begin
                  Move((Buffer + Offset)^, Pointer(Stream), SizeOf(Pointer));
                  Stream.Size := 0;
                  Stream.Position := 0;
                  GetBlobField(Fields[Index], Stream);
                end
             else if (Fields[Index].DataType in UNSUPPORTED) then
                     raise Exception.Create('TGXBaseDataset.RecordToBuffer - '+FT_NOT_SUPPORTED);
      end;
    end;
  DoAfterGetFieldValue;
end;

procedure TGXBaseDataset.DoDeleteRecord;
begin
  //Nothing in base class
end;

function TGXBaseDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuffer: PChar;
  Offset: Integer;
  TempDouble: Double;
  Data: TDateTimeRec;
  TimeStamp: TTimeStamp;
  TempBool: WordBool;
begin
  Result := false;
  if not FisOpen then exit;
  RecBuffer := GetActiveRecordBuffer;
  if RecBuffer = nil then exit;
  if Buffer = nil then
    begin
    //Dataset checks if field is null by passing a nil buffer
    //Tell it is not null by passing back a result of True
      Result := True;
      exit;
    end;
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
    begin
      inc(RecBuffer, FStartCalculated + Field.Offset);
      if (RecBuffer[0] = #0) or (Buffer = nil) then
        exit
      else
        Move(RecBuffer[1], Buffer^, Field.DataSize);
//org        CopyMemory(Buffer, @RecBuffer[1], Field.DataSize); Windows.pas
    end
  else // not fkCalculated or fkLookup
    begin
      Offset := GetFieldOffset(Field);
      case Field.DataType of
        ftInteger, ftTime, ftDate: Move((RecBuffer + Offset)^, Integer(Buffer^), SizeOf(Integer));
        ftLargeInt : Move((RecBuffer + Offset)^, LargeInt(Buffer^), SizeOf(LargeInt));
        ftBoolean:
          begin
            Move((RecBuffer + Offset)^, TempBool, SizeOf(WordBool));
            Move(TempBool, WordBool(Buffer^), SizeOf(WordBool));
          end;
        ftString: StrLCopy(Buffer, PChar(RecBuffer + Offset), StrLen(PChar(RecBuffer + Offset)));
        ftCurrency, ftFloat: Move((RecBuffer + Offset)^, Double(Buffer^), sizeof(Double));
        ftDateTime:
          begin
            Move((RecBuffer + Offset)^, TempDouble, SizeOf(Double));
// leave this. it is fixed in new versions of FCL            
//orig.            TimeStamp := DateTimeToTimeStamp(TempDouble);
//orig.            Data.DateTime := TimeStampToMSecs(TimeStamp);
            Data.DateTime := TempDouble;
            Move(Data, Buffer^, SizeOf(TDateTimeRec));
          end
        else if (not Field.IsBlob) and (Field.DataType in UNSUPPORTED) then
                raise Exception.Create('TGXBaseDataset.GetFieldData - '+FT_NOT_SUPPORTED);
      end; // case
    end;
  Result := True;
end;

procedure TGXBaseDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
  Offset: Integer;
  RecBuffer: Pchar;
  TempDouble: Double;
  Data: TDateTimeRec;
  TimeStamp: TTimeStamp;
  TempBool: WordBool;
begin
  if not Active then
    exit;
  RecBuffer := GetActiveRecordBuffer;
  if RecBuffer = nil then
    exit;
  if Buffer = nil then
    exit;
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
    begin
      Inc(RecBuffer, FStartCalculated + Field.Offset);
      Boolean(RecBuffer[0]) := (Buffer <> nil);
      if Boolean(RecBuffer[0]) then
         Move(Buffer^, RecBuffer[1], Field.DataSize);
//        CopyMemory(@RecBuffer[1], Buffer, Field.DataSize); Windows.pas
    end
  else
    begin
      Offset := GetFieldOffset(Field);
      case Field.DataType of
        ftInteger, ftDate, ftTime: Move(Integer(Buffer^), (RecBuffer + Offset)^, SizeOf(Integer));
        ftLargeInt : Move(LargeInt(Buffer^), (RecBuffer + Offset)^, SizeOf(LargeInt));
        ftBoolean:
          begin
            Move(WordBool(Buffer^), TempBool, sizeof(WordBool));
            Move(TempBool, (RecBuffer + Offset)^, sizeof(WordBool));
          end;
        ftString: StrLCopy(PChar(RecBuffer + Offset), Buffer, StrLen(PChar(Buffer)));
        ftDateTime:
          begin
            Data := TDateTimeRec(Buffer^);
// leave this. it is fixed in new versions of FCL            
//orig.            TimeStamp := MSecsToTimeStamp(Data.DateTime);
//orig.            TempDouble := TimeStampToDateTime(TimeStamp);
            TempDouble := Data.DateTime;
            Move(TempDouble, (RecBuffer + Offset)^, SizeOf(TempDouble));
          end;
        ftFloat, ftCurrency: Move(Double(Buffer^), (RecBuffer + Offset)^, SizeOf(Double));
        else if (not Field.IsBlob) and (Field.DataType in UNSUPPORTED) then
                raise Exception.Create('TGXBaseDataset.SetFieldData - '+FT_NOT_SUPPORTED);
      end; // case
    end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Ptrint(Field));
end;

procedure TGXBaseDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if BookMarkSize > 0 then
     AllocateBookMark(PRecordInfo(Buffer + GetDataSize)^.RecordID, Data);
//     Data := PRecordInfo(Buffer + GetDataSize)^.Bookmark;
end;

function TGXBaseDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecordInfo(Buffer + GetDataSize)^.BookMarkFlag;
end;

procedure TGXBaseDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if PRecordInfo(Buffer + GetDataSize)^.BookMark = nil then
     GetMem(PRecordInfo(Buffer + GetDataSize)^.BookMark, GetBookMarkSize);
  Move(PRecordInfo(Buffer + GetDataSize)^.BookMark^, Data, GetBookMarkSize);
end;

procedure TGXBaseDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecordInfo(Buffer + GetDataSize)^.BookMarkFlag := Value;
end;

procedure TGXBaseDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  DoGotoBookMark(ABookMark);
end;

function TGXBaseDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TGXBlobStream.Create(Field as TBlobField, Mode);
end;

//************************** TGXBlobStream ***************************************

constructor TGXBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  inherited Create;
  FField := Field;
  FMode := Mode;
  FDataSet := FField.DataSet as TGXBaseDataset;
  if Mode <> bmWrite then
    LoadBlobData;
end;

destructor TGXBlobStream.Destroy;
begin
  if FModified then
    SaveBlobData;
  inherited Destroy;
end;

function TGXBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := inherited Read(Buffer, Count);
  FOpened := True;
end;

function TGXBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

procedure TGXBlobStream.LoadBlobData;
var
  Stream: TMemoryStream;
  Offset: Integer;
  RecBuffer: PChar;
begin
  Self.Size := 0;
  RecBuffer := FDataset.GetActiveRecordBuffer;
  if RecBuffer <> nil then
    begin
      Offset := FDataset.GetFieldOffset(FField);
      Move((RecBuffer + Offset)^, Pointer(Stream), SizeOf(Pointer));
      Self.CopyFrom(Stream, 0);
    end;
  Position := 0;
end;

procedure TGXBlobStream.SaveBlobData;
var
  Stream: TMemoryStream;
  Offset: Integer;
  RecBuffer: Pchar;
begin
  RecBuffer := FDataset.GetActiveRecordBuffer;
  if RecBuffer <> nil then
    begin
      Offset := FDataset.GetFieldOffset(FField);
      Move((RecBuffer + Offset)^, Pointer(Stream), SizeOf(Pointer));
      Stream.Size := 0;
      Stream.CopyFrom(Self, 0);
      Stream.Position := 0;
    end;
  FModified := False;
end;

procedure TGXBaseDataset.SetFiltered(Value: Boolean);
begin
  inherited;
  First;
end;

initialization

  OldTimeSeparator := TimeSeparator;
  OldDateSeparator := DateSeparator;

  if TimeSeparator <> ':' then
     TimeSeparator := ':';

  if DateSeparator <> '.' then
     DateSeparator := '.';
      
finalization

  TimeSeparator := OldTimeSeparator;
  DateSeparator := OldDateSeparator;
   
end.

